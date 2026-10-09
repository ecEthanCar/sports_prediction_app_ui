#!/usr/bin/env python3
"""
Build pooled 2015/16 datasets from StatsBomb open data for the four complete
leagues: Premier League, La Liga, Serie A, Ligue 1. (The 2015/16 Bundesliga
has only 34 matches in the open data and is excluded.)

Shot rows follow build_epl_shots.py (same shot unit, coordinate frame, and
on-pitch reconstruction from Starting XI, substitutions, and red cards).

Run by the pipeline (targets::tar_make() in the project root), or directly
with `python3 build/build_shots.py` from the project root.

Outputs:
  data/raw/shots_1516.csv    one row per shot event (all shot types; filtering is
                         done in R), with the stint it belongs to
  data/raw/matches_1516.csv  one row per match
  data/raw/stints_1516.csv   one row per stint: a stretch of play with the same
                         players on the pitch, ending at a substitution, a red
                         card, or the end of a half
  data/raw/segments_1516.csv one row per segment: stints split further at goals
                         and at 15-minute game-clock edges, so the score and
                         time bin are constant within each segment
  data/raw/players_1516.csv  one row per player and team, with primary position
                         (most frequent position across matches) and a
                         goalkeeper flag

Stint conventions:
  - times are seconds elapsed within the period, from event timestamps
  - temporary absences ("Player Off" / "Player On") are ignored
  - zero-length stints (e.g. halftime substitutions recorded at 0:00 of the
    second half) are dropped; a zero-length stint containing a shot is an error
  - home_goals / away_goals are the score at the start of the stint or
    segment, counting goals from shots of any type and own goals
  - segments end at goals as well as lineup changes and 15-minute clock edges
    (0-15, 15-30, 30-45+ within each period, stoppage time in the last bin);
    a goal's shot belongs to the segment before the goal

Coordinates: origin at the centre of the attacking goal; shot_y is distance
from the goal line toward midfield, shot_x is the lateral offset (positive is
the shooter's right). Units are StatsBomb pitch units (~yards).

Raw match lists, lineups, and slimmed event files are cached under
data/statsbomb_raw/. Slimmed events are cached in events_slim_v2/ (the earlier
events_slim/ folder lacks half boundaries and own goals and can be deleted).
"""

import concurrent.futures as cf
import csv
import json
import math
import os
import re
import time
import unicodedata
from collections import Counter, defaultdict

import requests

BASE = "https://raw.githubusercontent.com/statsbomb/open-data/master/data"
SEASON_ID = 27
SEASON_LABEL = "2015/16"
YY = "16"
LEAGUES = {2: "Premier League", 11: "La Liga", 12: "Serie A", 7: "Ligue 1"}

OUT_DIR = os.path.join("data", "raw")
CACHE_DIR = os.path.join("data", "statsbomb_raw")   # download cache, unchanged location
SLIM_DIR = "events_slim_v2"
SHOTS_OUT = os.path.join(OUT_DIR, "shots_1516.csv")
MATCHES_OUT = os.path.join(OUT_DIR, "matches_1516.csv")
STINTS_OUT = os.path.join(OUT_DIR, "stints_1516.csv")
SEGMENTS_OUT = os.path.join(OUT_DIR, "segments_1516.csv")
CLOCK_EDGES = (900.0, 1800.0)   # seconds after each period starts
PLAYERS_OUT = os.path.join(OUT_DIR, "players_1516.csv")
WORKERS = 8

RED_CARDS = {"Red Card", "Second Yellow"}
SLIM_TYPES = {"Starting XI", "Substitution", "Shot", "Half Start", "Half End",
              "Own Goal Against"}

# Wells's EPL codes, kept for continuity; other teams get generated codes.
TEAM_ABBR = {
    "Arsenal": "ARS", "Aston Villa": "AVL", "AFC Bournemouth": "BOU",
    "Bournemouth": "BOU", "Chelsea": "CHE", "Crystal Palace": "CRY",
    "Everton": "EVE", "Leicester City": "LEI", "Liverpool": "LIV",
    "Manchester City": "MCI", "Manchester United": "MUN",
    "Newcastle United": "NEW", "Norwich City": "NOR", "Southampton": "SOU",
    "Stoke City": "STK", "Sunderland": "SUN", "Swansea City": "SWA",
    "Tottenham Hotspur": "TOT", "Watford": "WAT",
    "West Bromwich Albion": "WBA", "West Ham United": "WHU",
}

SHOT_COLUMNS = (
    ["league", "season", "game_id", "event_id", "match_date", "period",
     "minute", "t_sec", "stint_id", "segment_id", "poss_num", "home_off", "off_team",
     "def_team", "shooter", "shooter_id", "outcome", "shot_made",
     "poss_goals", "poss_end", "shot_x", "shot_y", "distance", "angle", "xg",
     "set_piece", "body_part", "technique", "play_pattern", "first_time",
     "under_pressure", "strength"]
    + [f"O{i}" for i in range(1, 12)] + [f"D{i}" for i in range(1, 12)]
)

MATCH_COLUMNS = ["league", "season", "game_id", "match_date", "home_team",
                 "away_team", "home_team_id", "away_team_id", "home_score",
                 "away_score"]

STINT_COLUMNS = (
    ["league", "season", "game_id", "stint_id", "stint_num", "period",
     "start_sec", "end_sec", "duration_min", "home_team", "away_team",
     "home_goals", "away_goals", "n_home", "n_away"]
    + [f"H{i}" for i in range(1, 12)] + [f"A{i}" for i in range(1, 12)]
)

SEGMENT_COLUMNS = (
    ["league", "season", "game_id", "segment_id", "segment_num", "stint_id",
     "period", "time_bin", "start_sec", "end_sec", "duration_min", "home_team",
     "away_team", "home_goals", "away_goals", "n_home", "n_away"]
    + [f"H{i}" for i in range(1, 12)] + [f"A{i}" for i in range(1, 12)]
)

PLAYER_COLUMNS = ["player_id", "player_name", "team", "league",
                  "primary_position", "is_goalkeeper"]


# ---------------------------------------------------------------- fetching

def fetch_json(session, url, tries=5):
    """GET a JSON URL with retries and exponential backoff."""
    for attempt in range(tries):
        try:
            r = session.get(url, timeout=60)
            if r.status_code == 200:
                return r.json()
            if r.status_code in (429, 500, 502, 503, 504):
                time.sleep(2 ** attempt)
                continue
            r.raise_for_status()
        except requests.RequestException:
            if attempt == tries - 1:
                raise
            time.sleep(2 ** attempt)
    raise RuntimeError(f"Failed to fetch {url} after {tries} attempts")


def write_json_atomic(path, data):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(data, f)
    os.replace(tmp, path)


def read_json(path):
    with open(path, encoding="utf-8") as f:
        return json.load(f)


def get_cached(session, rel_path):
    """Fetch BASE/rel_path, caching the raw JSON under CACHE_DIR."""
    local = os.path.join(CACHE_DIR, rel_path)
    if os.path.exists(local):
        return read_json(local)
    data = fetch_json(session, f"{BASE}/{rel_path}")
    write_json_atomic(local, data)
    return data


def event_card(e):
    """Card name on a foul or bad-behaviour event, or None."""
    card = ((e.get("foul_committed") or {}).get("card")
            or (e.get("bad_behaviour") or {}).get("card")
            or {})
    return card.get("name")


def slim_events(events):
    """Keep only the events the build uses."""
    return [e for e in events
            if e["type"]["name"] in SLIM_TYPES or event_card(e) in RED_CARDS]


def get_events_slim(session, match_id):
    local = os.path.join(CACHE_DIR, SLIM_DIR, f"{match_id}.json")
    if os.path.exists(local):
        return read_json(local)
    events = fetch_json(session, f"{BASE}/events/{match_id}.json")
    slim = slim_events(events)
    write_json_atomic(local, slim)
    return slim


def ts_seconds(ts):
    """'HH:MM:SS.fff' (time within period) -> seconds."""
    h, m, s = ts.split(":")
    return int(h) * 3600 + int(m) * 60 + float(s)


# ---------------------------------------------------------- team codes

def ascii_upper(s):
    s = unicodedata.normalize("NFKD", s).encode("ascii", "ignore").decode()
    return re.sub(r"[^A-Za-z]", "", s).upper()


def assign_abbreviations(team_names):
    """team_id -> unique code. Wells's EPL codes where available; otherwise
    the first three letters of the name, with a digit suffix on collision.
    Deterministic: teams processed in team_id order."""
    abbr, used = {}, set()
    for tid in sorted(team_names):
        name = team_names[tid]
        if name in TEAM_ABBR:
            abbr[tid] = TEAM_ABBR[name]
            used.add(TEAM_ABBR[name])
    for tid in sorted(team_names):
        if tid in abbr:
            continue
        base = (ascii_upper(team_names[tid]) + "XXX")[:3]
        code, n = base, 1
        while code in used:
            n += 1
            code = f"{base[:2]}{n}"
        abbr[tid] = code
        used.add(code)
    return abbr


# ------------------------------------------------------- per-match build

def process_match(session, match, league, abbr):
    mid = match["match_id"]
    events = sorted(get_events_slim(session, mid), key=lambda x: x["index"])
    lineups = get_cached(session, f"lineups/{mid}.json")

    home_id = match["home_team"]["home_team_id"]
    away_id = match["away_team"]["away_team_id"]
    team_name = {home_id: match["home_team"]["home_team_name"],
                 away_id: match["away_team"]["away_team_name"]}

    # Starting position from the Starting XI, used if the lineup file has no positions
    start_pos = {}
    for e in events:
        if e["type"]["name"] == "Starting XI":
            for p in e["tactics"]["lineup"]:
                start_pos[p["player"]["id"]] = p["position"]["name"]

    names, player_rows = {}, []
    for team in lineups:
        for p in team["lineup"]:
            display = p["player_nickname"] or p["player_name"]
            names[p["player_id"]] = display
            positions = p.get("positions") or []
            pos = positions[0]["position"] if positions else start_pos.get(p["player_id"])
            player_rows.append({"player_id": p["player_id"], "player_name": display,
                                "team": team_name.get(team["team_id"], team["team_name"]),
                                "league": league, "position": pos})
    for e in events:  # fallback for anyone missing from the lineup file
        if "player" in e:
            names.setdefault(e["player"]["id"], e["player"]["name"])
        if e["type"]["name"] == "Starting XI":
            for p in e["tactics"]["lineup"]:
                names.setdefault(p["player"]["id"], p["player"]["name"])
        if e["type"]["name"] == "Substitution":
            rep = e["substitution"]["replacement"]
            names.setdefault(rep["id"], rep["name"])

    def label(team_id, player_id):
        return f"{abbr[team_id]}{YY} {names[player_id]}"

    # Period boundaries: earliest Half Start and latest Half End per period
    starts, ends = {}, {}
    for e in events:
        if e["type"]["name"] == "Half Start":
            starts[e["period"]] = min(starts.get(e["period"], math.inf), ts_seconds(e["timestamp"]))
        elif e["type"]["name"] == "Half End":
            ends[e["period"]] = max(ends.get(e["period"], -math.inf), ts_seconds(e["timestamp"]))
    if set(starts) != {1, 2} or set(ends) != {1, 2}:
        raise RuntimeError(f"Match {mid}: expected periods 1 and 2; "
                           f"Half Start in {sorted(starts)}, Half End in {sorted(ends)}")

    on_pitch = defaultdict(set)              # team_id -> set of player_id
    score = {home_id: 0, away_id: 0}
    segs, rows = [], []
    cur = None                               # the open segment
    n_stint, n_seg = [0], [0]
    edges = []                               # clock edges left in the open period

    def open_seg(period, t, new_stint):
        if new_stint:
            n_stint[0] += 1
        n_seg[0] += 1
        return {"stint": n_stint[0], "seg": n_seg[0], "period": period, "start": t,
                "home": frozenset(on_pitch[home_id]), "away": frozenset(on_pitch[away_id]),
                "hg": score[home_id], "ag": score[away_id], "n_shots": 0}

    def close_seg(seg, t):
        seg["end"] = t
        segs.append(seg)

    def cross_edges(period, t):
        """Close and reopen the segment at every clock edge at or before t."""
        nonlocal cur
        while cur is not None and cur["period"] == period and edges and t >= edges[0]:
            edge = edges.pop(0)
            close_seg(cur, edge)
            cur = open_seg(period, edge, new_stint=False)

    def split_after_goal(period, t):
        """Close and reopen the segment so the new one starts with the new score."""
        nonlocal cur
        if cur is not None:
            close_seg(cur, t)
            cur = open_seg(period, t, new_stint=False)

    for e in events:
        etype = e["type"]["name"]
        period = e["period"]
        t = ts_seconds(e["timestamp"])
        team_id = e["team"]["id"]

        if etype == "Starting XI":
            on_pitch[team_id] = {p["player"]["id"] for p in e["tactics"]["lineup"]}
            continue
        if etype == "Half Start":
            if cur is None:
                cur = open_seg(period, starts[period], new_stint=True)
                edges = [starts[period] + x for x in CLOCK_EDGES if starts[period] + x < ends[period]]
            continue
        if etype == "Half End":
            if cur is not None:
                cross_edges(period, ends[period] - 1e-9)
                close_seg(cur, ends[period])
                cur = None
            continue

        cross_edges(period, t)

        if etype == "Own Goal Against":       # team_id conceded; credit the opponent
            score[away_id if team_id == home_id else home_id] += 1
            split_after_goal(period, t)
            continue

        # Lineup changes start a new stint, but only if the lineup actually changes
        before = (frozenset(on_pitch[home_id]), frozenset(on_pitch[away_id]))
        if etype == "Substitution":
            on_pitch[team_id].discard(e["player"]["id"])
            on_pitch[team_id].add(e["substitution"]["replacement"]["id"])
        elif event_card(e) in RED_CARDS:
            on_pitch[team_id].discard(e["player"]["id"])
        after = (frozenset(on_pitch[home_id]), frozenset(on_pitch[away_id]))
        if after != before and cur is not None:
            close_seg(cur, t)
            cur = open_seg(period, t, new_stint=True)

        if etype != "Shot":
            continue
        if period > 2:
            raise RuntimeError(f"Match {mid}: shot in period {period}")
        if cur is None or cur["period"] != period:
            raise RuntimeError(f"Match {mid}: shot outside any segment at event {e['id']}")
        if (cur["home"], cur["away"]) != after:
            raise RuntimeError(f"Match {mid}: segment lineup differs from on-pitch lineup at event {e['id']}")

        shot = e["shot"]
        off_id = team_id
        def_id = away_id if off_id == home_id else home_id
        if len(on_pitch[off_id]) > 11 or len(on_pitch[def_id]) > 11:
            raise RuntimeError(f"Match {mid}: more than 11 players on pitch at event {e['id']}")

        x, y = e["location"][0], e["location"][1]
        shot_y = round(120.0 - x, 1)
        shot_x = round(y - 40.0, 1)
        dist = math.hypot(shot_x, shot_y)
        angle = math.degrees(math.atan2(abs(shot_x), shot_y))

        o_labels = sorted(label(off_id, p) for p in on_pitch[off_id])
        d_labels = sorted(label(def_id, p) for p in on_pitch[def_id])
        o_labels += [""] * (11 - len(o_labels))
        d_labels += [""] * (11 - len(d_labels))

        rows.append({
            "league": league,
            "season": SEASON_LABEL,
            "game_id": mid,
            "event_id": e["id"],
            "match_date": match["match_date"],
            "period": period,
            "minute": e["minute"],
            "t_sec": round(t, 3),
            "stint_id": f"{mid}_{cur['stint']}",
            "segment_id": f"{mid}_{cur['seg']}",
            "poss_num": e["possession"],
            "home_off": int(off_id == home_id),
            "off_team": team_name[off_id],
            "def_team": team_name[def_id],
            "shooter": label(off_id, e["player"]["id"]),
            "shooter_id": e["player"]["id"],
            "outcome": shot["outcome"]["name"],
            "shot_made": int(shot["outcome"]["name"] == "Goal"),
            "poss_goals": 0,   # filled below per possession
            "poss_end": 0,     # filled below per possession
            "shot_x": shot_x,
            "shot_y": shot_y,
            "distance": round(dist, 1),
            "angle": round(angle, 1),
            "xg": round(shot["statsbomb_xg"], 6),
            "set_piece": shot["type"]["name"],
            "body_part": shot["body_part"]["name"],
            "technique": shot["technique"]["name"],
            "play_pattern": e["play_pattern"]["name"],
            "first_time": int(bool(shot.get("first_time"))),
            "under_pressure": int(bool(e.get("under_pressure"))),
            "strength": f"{len(on_pitch[off_id])}v{len(on_pitch[def_id])}",
            **{f"O{i+1}": o_labels[i] for i in range(11)},
            **{f"D{i+1}": d_labels[i] for i in range(11)},
        })
        cur["n_shots"] += 1
        if shot["outcome"]["name"] == "Goal":
            score[off_id] += 1
            split_after_goal(period, t)

    if cur is not None:
        raise RuntimeError(f"Match {mid}: segment left open (missing Half End)")

    # Drop zero-length segments; they must not contain shots
    kept = []
    for g in segs:
        if g["end"] - g["start"] <= 0:
            if g["n_shots"]:
                raise RuntimeError(f"Match {mid}: zero-length segment {g['seg']} contains shots")
            continue
        kept.append(g)

    # Integrity: segments cover each period contiguously; sensible player counts;
    # each segment lies within one clock bin
    for p in (1, 2):
        ps = [g for g in kept if g["period"] == p]
        if not ps:
            raise RuntimeError(f"Match {mid}: no segments in period {p}")
        if abs(ps[0]["start"] - starts[p]) > 1e-6 or abs(ps[-1]["end"] - ends[p]) > 1e-6:
            raise RuntimeError(f"Match {mid}: segments do not span period {p}")
        for a, b in zip(ps, ps[1:]):
            if abs(b["start"] - a["end"]) > 1e-6:
                raise RuntimeError(f"Match {mid}: gap or overlap between segments {a['seg']} and {b['seg']}")
    for g in kept:
        if not (7 <= len(g["home"]) <= 11 and 7 <= len(g["away"]) <= 11):
            raise RuntimeError(f"Match {mid}: segment {g['seg']} has {len(g['home'])} / {len(g['away'])} players")
        g["bin"] = min(int((g["start"] - starts[g["period"]]) // 900), 2)
        bin_end = starts[g["period"]] + 900 * (g["bin"] + 1)
        if g["bin"] < 2 and g["end"] > bin_end + 1e-6:
            raise RuntimeError(f"Match {mid}: segment {g['seg']} crosses a clock edge")

    # Stints = consecutive kept segments with the same lineup
    stints = []
    for g in kept:
        if stints and stints[-1]["num"] == g["stint"]:
            stints[-1]["end"] = g["end"]
        else:
            stints.append({"num": g["stint"], "period": g["period"], "start": g["start"],
                           "end": g["end"], "home": g["home"], "away": g["away"],
                           "hg": g["hg"], "ag": g["ag"]})

    by_poss = defaultdict(list)
    for r in rows:
        by_poss[r["poss_num"]].append(r)
    for poss_rows in by_poss.values():
        poss_rows[-1]["poss_end"] = 1
        g_total = sum(r["shot_made"] for r in poss_rows)
        for r in poss_rows:
            r["poss_goals"] = g_total

    def padded(ids):
        v = [str(x) for x in sorted(ids)]
        return v + [""] * (11 - len(v))

    def common(x):
        h, a = padded(x["home"]), padded(x["away"])
        return {
            "league": league,
            "season": SEASON_LABEL,
            "game_id": mid,
            "period": x["period"],
            "start_sec": round(x["start"], 3),
            "end_sec": round(x["end"], 3),
            "duration_min": round((x["end"] - x["start"]) / 60.0, 4),
            "home_team": team_name[home_id],
            "away_team": team_name[away_id],
            "home_goals": x["hg"],
            "away_goals": x["ag"],
            "n_home": len(x["home"]),
            "n_away": len(x["away"]),
            **{f"H{i+1}": h[i] for i in range(11)},
            **{f"A{i+1}": a[i] for i in range(11)},
        }

    bin_labels = ["0-15", "15-30", "30-45+"]
    stint_rows = [{**common(x), "stint_id": f"{mid}_{x['num']}", "stint_num": x["num"]}
                  for x in stints]
    segment_rows = [{**common(g), "segment_id": f"{mid}_{g['seg']}", "segment_num": g["seg"],
                     "stint_id": f"{mid}_{g['stint']}",
                     "time_bin": f"P{g['period']} {bin_labels[g['bin']]}"}
                    for g in kept]

    match_row = {
        "league": league,
        "season": SEASON_LABEL,
        "game_id": mid,
        "match_date": match["match_date"],
        "home_team": team_name[home_id],
        "away_team": team_name[away_id],
        "home_team_id": home_id,
        "away_team_id": away_id,
        "home_score": match.get("home_score"),
        "away_score": match.get("away_score"),
    }
    return rows, match_row, stint_rows, segment_rows, player_rows


# ------------------------------------------------------------------ main

def write_csv(path, columns, rows):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    with open(path, "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=columns)
        w.writeheader()
        w.writerows(rows)


def main():
    session = requests.Session()
    all_matches = []
    for comp_id, league in LEAGUES.items():
        ms = get_cached(session, f"matches/{comp_id}/{SEASON_ID}.json")
        print(f"{league}: {len(ms)} matches", flush=True)
        all_matches += [(league, m) for m in ms]

    team_names = {}
    for _, m in all_matches:
        team_names[m["home_team"]["home_team_id"]] = m["home_team"]["home_team_name"]
        team_names[m["away_team"]["away_team_id"]] = m["away_team"]["away_team_name"]
    abbr = assign_abbreviations(team_names)

    def work(item):
        league, m = item
        return process_match(requests.Session(), m, league, abbr)

    shot_rows, match_rows, stint_rows, segment_rows, player_rows = [], [], [], [], []
    with cf.ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for i, (rows, mrow, srows, grows, prows) in enumerate(ex.map(work, all_matches), 1):
            shot_rows.extend(rows)
            match_rows.append(mrow)
            stint_rows.extend(srows)
            segment_rows.extend(grows)
            player_rows.extend(prows)
            if i % 100 == 0:
                print(f"  {i}/{len(all_matches)} matches processed", flush=True)

    # Integrity checks before writing
    if len(match_rows) != len(all_matches):
        raise RuntimeError("Processed match count does not equal input match count")
    event_ids = [r["event_id"] for r in shot_rows]
    if len(set(event_ids)) != len(event_ids):
        raise RuntimeError("Duplicated shot event_id across matches")
    stint_ids = [r["stint_id"] for r in stint_rows]
    if len(set(stint_ids)) != len(stint_ids):
        raise RuntimeError("Duplicated stint_id")
    missing = {r["stint_id"] for r in shot_rows} - set(stint_ids)
    if missing:
        raise RuntimeError(f"{len(missing)} shot stint_ids have no stint row")
    segment_ids = [r["segment_id"] for r in segment_rows]
    if len(set(segment_ids)) != len(segment_ids):
        raise RuntimeError("Duplicated segment_id")
    missing = {r["segment_id"] for r in shot_rows} - set(segment_ids)
    if missing:
        raise RuntimeError(f"{len(missing)} shot segment_ids have no segment row")
    if {r["stint_id"] for r in segment_rows} != set(stint_ids):
        raise RuntimeError("Segments and stints cover different stint_ids")

    # One row per player and team; primary position = most frequent position
    # across matches (ties broken alphabetically); "Unknown" if never recorded
    players, pos_counts = {}, defaultdict(Counter)
    for p in player_rows:
        key = (p["player_id"], p["team"])
        players.setdefault(key, {k: p[k] for k in ("player_id", "player_name", "team", "league")})
        if p["position"]:
            pos_counts[key][p["position"]] += 1
    player_rows = []
    for key, base in players.items():
        c = pos_counts.get(key)
        primary = sorted(c.items(), key=lambda kv: (-kv[1], kv[0]))[0][0] if c else "Unknown"
        player_rows.append({**base, "primary_position": primary,
                            "is_goalkeeper": int(primary == "Goalkeeper")})
    player_rows.sort(key=lambda p: (p["league"], p["team"], p["player_name"]))

    # Stable sorts keep within-match order
    shot_rows.sort(key=lambda r: (r["league"], r["match_date"], r["game_id"]))
    match_rows.sort(key=lambda r: (r["league"], r["match_date"], r["game_id"]))
    match_date = {r["game_id"]: r["match_date"] for r in match_rows}
    stint_rows.sort(key=lambda r: (r["league"], match_date[r["game_id"]], r["game_id"],
                                   r["period"], r["start_sec"]))
    segment_rows.sort(key=lambda r: (r["league"], match_date[r["game_id"]], r["game_id"],
                                     r["period"], r["start_sec"]))

    write_csv(SHOTS_OUT, SHOT_COLUMNS, shot_rows)
    write_csv(MATCHES_OUT, MATCH_COLUMNS, match_rows)
    write_csv(STINTS_OUT, STINT_COLUMNS, stint_rows)
    write_csv(SEGMENTS_OUT, SEGMENT_COLUMNS, segment_rows)
    write_csv(PLAYERS_OUT, PLAYER_COLUMNS, player_rows)
    print(f"Wrote {SHOTS_OUT}: {len(shot_rows)} shots", flush=True)
    print(f"Wrote {MATCHES_OUT}: {len(match_rows)} matches", flush=True)
    print(f"Wrote {STINTS_OUT}: {len(stint_rows)} stints", flush=True)
    print(f"Wrote {SEGMENTS_OUT}: {len(segment_rows)} segments", flush=True)
    print(f"Wrote {PLAYERS_OUT}: {len(player_rows)} player-team rows", flush=True)


if __name__ == "__main__":
    main()
