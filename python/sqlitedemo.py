import sqlite3
conn = sqlite3.connect("demodb.db")
cursor = conn.cursor()

cursor.execute("""CREATE TABLE albums
               (title text, artist text, release_data text,
               publisher text, media_type text)
               """)
