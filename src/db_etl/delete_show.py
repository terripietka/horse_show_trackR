import sqlite3

conn = sqlite3.connect("C:/Users/Terri/Documents/github_projects/ILRHA_Points_2025/db/ilrha2025v0428.db")
cursor = conn.cursor()

show_id_to_delete = 2

# Delete related results first (foreign key dependencies)
cursor.execute("DELETE FROM results WHERE show_id = ?", (show_id_to_delete,))
cursor.execute("DELETE FROM show_judges WHERE show_id = ?", (show_id_to_delete,))
cursor.execute("DELETE FROM shows WHERE show_id = ?", (show_id_to_delete,))

conn.commit()
conn.close()

print(f" Deleted all entries related to show_id = {show_id_to_delete}")
