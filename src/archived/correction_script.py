import sqlite3

import os
print("Working directory:", os.getcwd())

conn = sqlite3.connect("../db/morha2025.db")
cursor = conn.cursor()

# Insert missing judge
cursor.execute('''
    INSERT OR IGNORE INTO judges (judge_id, judge_name)
    VALUES (?, ?)
''', (11, "Amy Marx"))

# Get show_id (or replace this if you already know it)
cursor.execute("SELECT show_id FROM shows WHERE show_name = ?", ("Spring Fling",))
show_id = cursor.fetchone()[0]

# Link judge to show
cursor.execute('''
    INSERT OR IGNORE INTO show_judges (show_id, judge_id)
    VALUES (?, ?)
''', (show_id, 11))

conn.commit()
conn.close()
