import sqlite3
# Connect to SQLite database
<<<<<<< HEAD
conn = sqlite3.connect(r'C:/Users/Terri/Documents/github_projects/MoRHA_2025/DB/morha_120125.db')
=======
conn = sqlite3.connect(r'C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/DB/ilrha2025v0428.db')
>>>>>>> d070d812b221131bd76768ae6615151486d39468
cursor = conn.cursor()

# --------------------- Create Tables --------------------- #

# People (owners & exhibitors combined)
cursor.execute('''
CREATE TABLE IF NOT EXISTS people (
    arha_no INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    is_generated BOOLEAN DEFAULT 0
)
''')

# Horses
cursor.execute('''
CREATE TABLE IF NOT EXISTS horses (
    arha_no INTEGER PRIMARY KEY,
    horse_name TEXT NOT NULL,
    owner_arha_no INTEGER,
    is_generated BOOLEAN DEFAULT 0,
    FOREIGN KEY (owner_arha_no) REFERENCES people(arha_no)
)
''')

# Judges
cursor.execute('''
CREATE TABLE IF NOT EXISTS judges (
    judge_id INTEGER PRIMARY KEY,
    judge_name TEXT NOT NULL UNIQUE
)
''')

# Shows
cursor.execute('''
CREATE TABLE IF NOT EXISTS shows (
    show_id INTEGER PRIMARY KEY AUTOINCREMENT,
    show_name TEXT NOT NULL,
    date TEXT NOT NULL,
    location TEXT NOT NULL
)
''')

# Show-Judges link
cursor.execute('''
CREATE TABLE IF NOT EXISTS show_judges (
    show_id INTEGER,
    judge_id INTEGER,
    PRIMARY KEY (show_id, judge_id),
    FOREIGN KEY (show_id) REFERENCES shows(show_id),
    FOREIGN KEY (judge_id) REFERENCES judges(judge_id)
)
''')

# Results
cursor.execute('''
CREATE TABLE IF NOT EXISTS results (
    result_id INTEGER PRIMARY KEY AUTOINCREMENT,
    show_id INTEGER,
    judge_id INTEGER,
    class_name TEXT NOT NULL,
    horse_arha_no INTEGER,
    exhibitor_arha_no INTEGER,
    placing INTEGER,
    entry_no INTEGER,  -- ← add this line
    FOREIGN KEY (show_id) REFERENCES shows(show_id),
    FOREIGN KEY (judge_id) REFERENCES judges(judge_id),
    FOREIGN KEY (horse_arha_no) REFERENCES horses(arha_no),
    FOREIGN KEY (exhibitor_arha_no) REFERENCES people(arha_no)
)
''')

conn.commit()
conn.close()