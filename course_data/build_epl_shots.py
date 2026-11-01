#!/usr/bin/env python3
"""
Construct EPL shot-level datasets from StatsBomb open data, in the style of
nba_shots.csv / wnba_shots.csv / nhl_corsi_xg.csv (see datasets_codebook.docx).

Unit of observation: the shot event (StatsBomb event type 16). Own goals are
not shot events and contribute no rows. Penalty-shootout periods do not occur
in league play; period <= 4 is enforced anyway.

Outputs:
  epl_shots_1516.csv  - Premier League 2015/16, all 380 matches
  epl_shots_0304.csv  - Premier League 2003/04, the 38 Arsenal matches
                        (the only 2003/04 matches in StatsBomb open data)

On-pitch personnel are reconstructed from Starting XI events, Substitution
events, and red cards (foul_committed.card or bad_behaviour.card in
{Red Card, Second Yellow}), applied in event order; each shot row records the
players on the pitch as of the shot event. O1-O11 / D1-D11 are an unordered
set, sorted alphabetically by label; blank slots are real deployments
(post-red-card shorthanded sides), not missing data.

Coordinates are converted from StatsBomb's 120x80 pitch frame (attacking
left-to-right, origin at the top-left corner) to an attacking-goal frame:
origin at the center of the attacking goal (StatsBomb (120, 40));
shot_y = 120 - x  (distance from the goal line toward midfield; midfield 60);
shot_x = y - 40   (lateral offset; touchlines at +/-40; positive is the
                   shooter's right, i.e. StatsBomb's increasing y).
Units are StatsBomb pitch units (~yards). distance = hypot(shot_x, shot_y);
angle = degrees(atan2(|shot_x|, shot_y)): 0 = head on, 90 = on the goal line.
"""

import concurrent.futures as cf
import csv
import math
import sys
import unicodedata
from collections import defaultdict

import requests

BASE = "https://raw.githubusercontent.com/statsbomb/open-data/master/data"
COMP_ID = 2  # Premier League

SEASONS = {
    27: dict(label="2015/16", yy="16", out="epl_shots_1516.csv"),
    44: dict(label="2003/04", yy="04", out="epl_shots_0304.csv"),
}

TEAM_ABBR = {
    "Arsenal": "ARS", "Aston Villa": "AVL", "AFC Bournemouth": "BOU",
    "Bournemouth": "BOU", "Birmingham City": "BIR", "Blackburn Rovers": "BLB",
    "Bolton Wanderers": "BOL", "Charlton Athletic": "CHA", "Chelsea": "CHE",
    "Crystal Palace": "CRY", "Everton": "EVE", "Fulham": "FUL",
    "Leeds United": "LEE", "Leicester City": "LEI", "Liverpool": "LIV",
    "Manchester City": "MCI", "Manchester United": "MUN",
    "Middlesbrough": "MID", "Newcastle United": "NEW", "Norwich City": "NOR",
    "Portsmouth": "POR", "Southampton": "SOU", "Stoke City": "STK",
    "Sunderland": "SUN", "Swansea City": "SWA", "Tottenham Hotspur": "TOT",
    "Watford": "WAT", "West Bromwich Albion": "WBA", "West Ham United": "WHU",
    "Wolverhampton Wanderers": "WOL",
}

RED_CARDS = {"Red Card", "Second Yellow"}

COLUMNS = (
    ["season", "game_id", "poss_num", "season_type", "home_off", "shooter",
     "outcome", "shot_made", "poss_goals", "poss_end", "shot_x", "shot_y",
     "distance", "angle", "xg", "set_piece", "body_part", "technique",
     "play_pattern", "first_time", "under_pressure", "strength"]
    + [f"O{i}" for i in range(1, 12)] + [f"D{i}" for i in range(1, 12)]
)


def get_json(session, url):
    r = session.get(url, timeout=60)
    r.raise_for_status()
    return r.json()


def abbr(team_name):
    if team_name not in TEAM_ABBR:
        sys.exit(f"No abbreviation defined for team: {team_name!r}")
    return TEAM_ABBR[team_name]


def display_names(lineups):
    """player_id -> nickname if present else full name, per match."""
    names = {}
    for team in lineups:
        for p in team["lineup"]:
            names[p["player_id"]] = p["player_nickname"] or p["player_name"]
    return names


def process_match(session, match, yy, season_label):
    mid = match["match_id"]
    events = get_json(session, f"{BASE}/events/{mid}.json")
    lineups = get_json(session, f"{BASE}/lineups/{mid}.json")
    names = display_names(lineups)

    home_id = match["home_team"]["home_team_id"]
    team_abbrs = {
        match["home_team"]["home_team_id"]: abbr(match["home_team"]["home_team_name"]),
        match["away_team"]["away_team_id"]: abbr(match["away_team"]["away_team_name"]),
    }

    def label(team_id, player_id):
        return f"{team_abbrs[team_id]}{yy} {names[player_id]}"

    on_pitch = defaultdict(set)  # team_id -> set of player_id
    rows = []

    for e in sorted(events, key=lambda x: x["index"]):
        etype = e["type"]["name"]
        team_id = e["team"]["id"]

        if etype == "Starting XI":
            on_pitch[team_id] = {p["player"]["id"] for p in e["tactics"]["lineup"]}
            continue
        if etype == "Substitution":
            on_pitch[team_id].discard(e["player"]["id"])
            on_pitch[team_id].add(e["substitution"]["replacement"]["id"])
            continue
        card = (e.get("foul_committed", {}).get("card", {}) or
                e.get("bad_behaviour", {}).get("card", {}))
        if card.get("name") in RED_CARDS:
            on_pitch[team_id].discard(e["player"]["id"])
            # no continue: a red card can, in principle, share an event with
            # nothing else we need, and Shot events never carry cards

        if etype != "Shot" or e["period"] > 4:
            continue

        shot = e["shot"]
        off_id = team_id
        def_id = next(t for t in team_abbrs if t != off_id)
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
            "season": season_label,
            "game_id": mid,
            "poss_num": e["possession"],
            "season_type": "Regular Season",
            "home_off": int(off_id == home_id),
            "shooter": label(off_id, e["player"]["id"]),
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

    # per-possession fields: last attempt flag and total goals in possession
    by_poss = defaultdict(list)
    for r in rows:
        by_poss[r["poss_num"]].append(r)
    for poss_rows in by_poss.values():
        poss_rows[-1]["poss_end"] = 1
        g = sum(r["shot_made"] for r in poss_rows)
        for r in poss_rows:
            r["poss_goals"] = g
    return rows


def build_season(season_id, meta, workers=16):
    session = requests.Session()
    matches = get_json(session, f"{BASE}/matches/{COMP_ID}/{season_id}.json")
    matches = sorted(matches, key=lambda m: (m["match_date"], m["match_id"]))
    print(f"{meta['label']}: {len(matches)} matches", flush=True)

    all_rows = []
    def work(m):
        s = requests.Session()
        return process_match(s, m, meta["yy"], meta["label"])

    done = 0
    with cf.ThreadPoolExecutor(max_workers=workers) as ex:
        for rows in ex.map(work, matches):
            all_rows.extend(rows)
            done += 1
            if done % 40 == 0:
                print(f"  {done}/{len(matches)} matches processed", flush=True)

    # stable order: by game then possession then original append order
    all_rows.sort(key=lambda r: (r["game_id"], r["poss_num"]))
    with open(meta["out"], "w", newline="", encoding="utf-8") as f:
        w = csv.DictWriter(f, fieldnames=COLUMNS)
        w.writeheader()
        w.writerows(all_rows)
    print(f"  wrote {meta['out']}: {len(all_rows)} rows", flush=True)
    return all_rows


if __name__ == "__main__":
    for sid, meta in SEASONS.items():
        build_season(sid, meta)
