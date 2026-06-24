# ---------------------- Imports ---------------------- #
import sqlite3
import csv
from pathlib import Path
import re

# ---------------------- Configurable Settings ---------------------- #
<<<<<<< HEAD
db_path = Path("C:/Users/Terri/Documents/github_projects/MoRHA_2025/DB/morha_120125.db")
csv_file = Path("C:/Users/Terri/Documents/github_projects/MoRHA_2025/data/generic_results_jonathan carpenter.csv")

show_name = "August"
show_date = "2025-11-14"
show_location = "LSL, MO"
judge_name = "Carpenter"
show_index = 4
=======
db_path = Path("C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/DB/ilrha2025v0428.db")
csv_file = Path("C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/data/generic_results_butch watson_march.csv")

show_name = "March"
show_date = "2025-03-21"
show_location = "Gifford, IL"
judge_name = "Watson"
show_index = 1
>>>>>>> d070d812b221131bd76768ae6615151486d39468
judge_index = 4
judge_id = show_index * 10 + judge_index

# ID generation trackers
generated_ids = {"people": 9000000, "horses": 9500000}
name_to_arha = {"people": {}, "horses": {}}

# ---------------------- Helpers ---------------------- #
def clean_name(s):
    return s.strip().title()

def parse_arha_no(value):
    try:
        return int(value)
    except:
        return None

def get_or_create_person(cursor, name):
    name_cleaned = clean_name(name)
    if name_cleaned in name_to_arha["people"]:
        return name_to_arha["people"][name_cleaned]

    cursor.execute("SELECT arha_no FROM people WHERE UPPER(name) = UPPER(?)", (name_cleaned,))
    result = cursor.fetchone()
    if result:
        arha_no = result[0]
    else:
        arha_no = generated_ids["people"]
        generated_ids["people"] += 1
        cursor.execute("INSERT INTO people (arha_no, name, is_generated) VALUES (?, ?, 1)", (arha_no, name_cleaned))

    name_to_arha["people"][name_cleaned] = arha_no
    return arha_no

def get_or_create_horse(cursor, name, owner_arha):
    name_cleaned = clean_name(name)
    if name_cleaned in name_to_arha["horses"]:
        return name_to_arha["horses"][name_cleaned]

    cursor.execute("SELECT arha_no FROM horses WHERE UPPER(horse_name) = UPPER(?)", (name_cleaned,))
    result = cursor.fetchone()
    if result:
        arha_no = result[0]
    else:
        print(f"[Info] New horse detected, generating ARHA number for: '{name_cleaned}'")
        arha_no = generated_ids["horses"]
        generated_ids["horses"] += 1
        cursor.execute("INSERT INTO horses (arha_no, horse_name, owner_arha_no, is_generated) VALUES (?, ?, ?, 1)", (arha_no, name_cleaned, owner_arha))

    name_to_arha["horses"][name_cleaned] = arha_no
    return arha_no

# ---------------------- Initialize Database Connection ---------------------- #
conn = sqlite3.connect(db_path)
cursor = conn.cursor()

# ---------------------- Set Correct ID Start Points ---------------------- #
cursor.execute("SELECT MAX(arha_no) FROM horses")
max_horse_arha = cursor.fetchone()[0]
if max_horse_arha is not None:
    generated_ids["horses"] = max(max_horse_arha + 1, generated_ids["horses"])

cursor.execute("SELECT MAX(arha_no) FROM people")
max_people_arha = cursor.fetchone()[0]
if max_people_arha is not None:
    generated_ids["people"] = max(max_people_arha + 1, generated_ids["people"])

print(f"[Init] Next generated horse ARHA: {generated_ids['horses']}")
print(f"[Init] Next generated person ARHA: {generated_ids['people']}")

# ---------------------- Preload Horses and People ---------------------- #
cursor.execute("SELECT arha_no, name FROM people")
for arha_no, name in cursor.fetchall():
    name_to_arha["people"][clean_name(name)] = arha_no

cursor.execute("SELECT arha_no, horse_name FROM horses")
for arha_no, horse_name in cursor.fetchall():
    name_to_arha["horses"][clean_name(horse_name)] = arha_no

# ---------------------- Init Show & Judge ---------------------- #
cursor.execute('SELECT show_id FROM shows WHERE show_name = ? AND date = ? AND location = ?', (show_name, show_date, show_location))
result = cursor.fetchone()
if result:
    show_id = result[0]
else:
    cursor.execute('INSERT INTO shows (show_name, date, location) VALUES (?, ?, ?)', (show_name, show_date, show_location))
    show_id = cursor.lastrowid

cursor.execute('INSERT OR IGNORE INTO judges (judge_id, judge_name) VALUES (?, ?)', (judge_id, judge_name))
cursor.execute('INSERT OR IGNORE INTO show_judges (show_id, judge_id) VALUES (?, ?)', (show_id, judge_id))

# ---------------------- Parse CSV ---------------------- #
with csv_file.open("r", encoding="utf-8") as f:
    reader = csv.reader(f)
    current_class = None

    for row in reader:
        if not row or len(row) < 9:
            continue

        first_col = row[0].strip()

        class_match = re.match(r"^\d+[a-zA-Z]?\s*-\s*(.+?)\s+Entries:", first_col)
        if class_match:
            current_class = class_match.group(1).strip()
            continue

        if len(row) > 1 and row[1].strip().lower() == "place":
            continue

        # Parse fields
        placing_raw = row[1].strip()
        entry_no_raw = row[2].strip()
        horse_name = clean_name(row[3])
        exhibitor_name = clean_name(row[5])
        owner_name = clean_name(row[7])

        # If owner_name is missing, set to 'Unknown'
        if not owner_name:
            owner_name = "Unknown"

        placing = int(float(placing_raw)) if placing_raw.replace(".", "", 1).isdigit() else None
        entry_no = int(float(entry_no_raw)) if entry_no_raw.replace(".", "", 1).isdigit() else None

        if not horse_name or not exhibitor_name or not owner_name:
            print(f"[Warning] Skipping row due to missing horse/exhibitor/owner: {row}")
            continue

        owner_id = get_or_create_person(cursor, owner_name)
        exhibitor_id = get_or_create_person(cursor, exhibitor_name)
        horse_id = get_or_create_horse(cursor, horse_name, owner_id)

        cursor.execute('''
            INSERT INTO results (
                show_id, judge_id, class_name, horse_arha_no,
                exhibitor_arha_no, placing, entry_no 
            ) VALUES (?, ?, ?, ?, ?, ?, ?)
        ''', (
            show_id, judge_id, current_class,
            horse_id, exhibitor_id, placing, entry_no
        ))

conn.commit()
conn.close()

print("Parsing complete: People, Horses, Results loaded.")
