import os
import mailconfig
import smtplib
import win32file
import win32con

info   = mailconfig.mailconfig()


ACTIONS = {
    1 : "Created",
    2 : "Deleted",
    3 : "Updated",
    4 : "Renamed from something",
    5 : "Renamed to something"
    }

FILE_LIST_DIRECTORY = 0x0001

path_to_watch = 'C:\\users\\mjbaker\\Documents\\github\\newshour\\Notifier'
hDir = win32file.CreateFile (
    path_to_watch,
    FILE_LIST_DIRECTORY,
    win32con.FILE_SHARE_READ | win32con.FILE_SHARE_WRITE | win32con.FILE_SHARE_DELETE,
    None,
    win32con.OPEN_EXISTING,
    win32con.FILE_FLAG_BACKUP_SEMANTICS,
    None
    )
while 1:
    results = win32file.ReadDirectoryChangesW (
        hDir,
        1024,
        True,
        win32con.FILE_NOTIFY_CHANGE_FILE_NAME |
        win32con.FILE_NOTIFY_CHANGE_DIR_NAME |
        win32con.FILE_NOTIFY_CHANGE_ATTRIBUTES |
        win32con.FILE_NOTIFY_CHANGE_SIZE |
        win32con.FILE_NOTIFY_CHANGE_LAST_WRITE |
        win32con.FILE_NOTIFY_CHANGE_SECURITY,
        None,
        None
    )
    for action, file in results:
        server = smtplib.SMTP( "smtp.gmail.com:587" )	
        server.starttls()
        server.login( info.mailaddress, info.password )
        server.sendmail(info.sender,info.receiver,info.message )
        server.quit()
        full_filename = os.path.join (path_to_watch, file)
        print(full_filename, ACTIONS.get (action, "Unknown"))