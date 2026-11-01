#!/usr/bin/env python3
"""
Count, for every segment and attacking team, the completed open-play passes
and carries that enter the final third and that enter the penalty box. These
are the frequent outcomes for Part B (the frequency ladder).

Needs data/raw/segments_1516.csv and matches_1516.csv from build_shots.py.
The slimmed event cache does not keep passes and carries, so each match's
full event file is downloaded once and reduced to the entries it contains;
only the reduced records are cached (data/statsbomb_raw/entries_v1/), so later
runs need no network. Run by the pipeline, or directly with
`python3 build/build_events.py` from the research folder.

Output:
  data/raw/segment_events_1516.csv  game_id, segment_id, team,
                                    final_third_entries, box_entries
  (rows only where a team has at least one entry; R fills the zeros)

Definitions (StatsBomb pitch, 120 x 80, attacking toward x = 120):
  final-third entry: start x < 80 and end x >= 80
  box entry:         end inside the penalty area (x >= 102, 18 <= y <= 62)
                     and start outside it
  Completed passes only (no outcome recorded), excluding corners, free kicks,
  kick-offs and goal kicks; carries are always completed. Periods 1 and 2 only.
  Each entry is assigned to the segment containing its period and time.
"""

import concurrent.futures as cf
import csv
import os
from collections import Counter, defaultdict

import requests

from build_shots import (BASE, CACHE_DIR, OUT_DIR, WORKERS, fetch_json, read_json,
                         ts_seconds, write_csv, write_json_atomic)

SEGMENTS_IN = os.path.join(OUT_DIR, "segments_1516.csv")
MATCHES_IN = os.path.join(OUT_DIR, "matches_1516.csv")
EVENTS_OUT = os.path.join(OUT_DIR, "segment_events_1516.csv")
ENTRY_DIR = "entries_v1"
EVENT_COLUMNS = ["game_id", "segment_id", "team", "final_third_entries", "box_entries"]

FINAL_THIRD_X = 80.0
BOX_X, BOX_Y_LO, BOX_Y_HI = 102.0, 18.0, 62.0
SET_PIECE_PASSES = {"Corner", "Free Kick", "Kick Off", "Goal Kick"}


def in_box(loc):
    return loc[0] >= BOX_X and BOX_Y_LO <= loc[1] <= BOX_Y_HI


def entries_from_events(events):
    """[period, seconds within period, team id, final-third entry, box entry]"""
    out = []
    for e in events:
        kind = e["type"]["name"]
        if kind == "Pass":
            p = e["pass"]
            if "outcome" in p:                         # incomplete, out, offside, unknown
                continue
            if (p.get("type") or {}).get("name") in SET_PIECE_PASSES:
                continue
            end = p.get("end_location")
        elif kind == "Carry":
            end = (e.get("carry") or {}).get("end_location")
        else:
            continue
        start = e.get("location")
        if not start or not end or e["period"] > 2:
            continue
        ft = int(start[0] < FINAL_THIRD_X <= end[0])
        box = int(in_box(end) and not in_box(start))
        if ft or box:
            out.append([e["period"], round(ts_seconds(e["timestamp"]), 3), e["team"]["id"], ft, box])
    return out


def get_entries(match_id):
    local = os.path.join(CACHE_DIR, ENTRY_DIR, f"{match_id}.json")
    if os.path.exists(local):
        return match_id, read_json(local)
    events = fetch_json(requests.Session(), f"{BASE}/events/{match_id}.json")
    entries = entries_from_events(events)
    write_json_atomic(local, entries)
    return match_id, entries


def read_csv_rows(path):
    with open(path, newline="", encoding="utf-8") as f:
        return list(csv.DictReader(f))


def main():
    matches = read_csv_rows(MATCHES_IN)
    team_of = {}                                       # (game_id, team id) -> team name
    for m in matches:
        g = int(m["game_id"])
        team_of[(g, int(m["home_team_id"]))] = m["home_team"]
        team_of[(g, int(m["away_team_id"]))] = m["away_team"]

    segs = defaultdict(list)                           # (game_id, period) -> segments in time order
    for s in read_csv_rows(SEGMENTS_IN):
        segs[(int(s["game_id"]), int(s["period"]))].append(
            (float(s["start_sec"]), float(s["end_sec"]), s["segment_id"]))
    for v in segs.values():
        v.sort()

    game_ids = sorted({int(m["game_id"]) for m in matches})
    counts = defaultdict(lambda: [0, 0])               # (game_id, segment_id, team) -> [ft, box]
    unplaced = Counter()
    with cf.ThreadPoolExecutor(max_workers=WORKERS) as ex:
        for i, (g, entries) in enumerate(ex.map(get_entries, game_ids), 1):
            for period, t, team_id, ft, box in entries:
                team = team_of.get((g, int(team_id)))
                if team is None:
                    raise RuntimeError(f"Match {g}: team id {team_id} not in the matches table")
                periods = segs.get((g, period))
                if not periods:
                    unplaced["no segments in period"] += 1
                    continue
                seg = None
                for start, end, sid in periods:
                    if start <= t < end:
                        seg = sid
                        break
                if seg is None:                        # before the first or at/after the last boundary
                    if t >= periods[-1][1] and t - periods[-1][1] <= 60:
                        seg = periods[-1][2]
                    elif t < periods[0][0] and periods[0][0] - t <= 60:
                        seg = periods[0][2]
                    else:
                        unplaced["outside segments"] += 1
                        continue
                c = counts[(g, seg, team)]
                c[0] += ft
                c[1] += box
            if i % 100 == 0:
                print(f"  {i}/{len(game_ids)} matches", flush=True)

    rows = [{"game_id": g, "segment_id": sid, "team": team,
             "final_third_entries": c[0], "box_entries": c[1]}
            for (g, sid, team), c in sorted(counts.items())]
    total_ft = sum(r["final_third_entries"] for r in rows)
    total_box = sum(r["box_entries"] for r in rows)
    placed = total_ft + total_box
    n_unplaced = sum(unplaced.values())
    if n_unplaced > 0.001 * max(placed, 1):
        raise RuntimeError(f"Too many entries outside any segment: {dict(unplaced)}")
    write_csv(EVENTS_OUT, EVENT_COLUMNS, rows)
    print(f"Wrote {EVENTS_OUT}: {total_ft} final-third entries, {total_box} box entries "
          f"from {len(game_ids)} matches; unplaced: {dict(unplaced) or 0}", flush=True)


if __name__ == "__main__":
    main()
