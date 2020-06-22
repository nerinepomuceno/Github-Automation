import os
import tkinter as tk

root= tk.Tk()

canvas1 = tk.Canvas(root, width = 300, height = 300, bg = 'gray90', relief = 'raised')
canvas1.pack()

local_dir=r"C:\Users\acer\Documents\Git Uploads"
remote_url="https://github.com/nerinepomuceno/Github-Automation.git"
#remote_url="https://github.com/nerinepomuceno/Projects.git"
message="automated"

def myCmd ():
    os.system('cmd /k "color a & cd ' + local_dir +' & git init & git add . & git commit -m "' + message +'" & git remote set-url origin ' + remote_url +' & git push -u origin master"')
#r' /k "color a & cd C:\Users\acer\Documents\Git Uploads &git init & git add . & git commit -m "automated" & git remote add origin https://github.com/nerinepomuceno/Github-Automation.git "')


button1 = tk.Button(text='      Run Command      ', command=myCmd, bg='green', fg='white', font=('helvetica', 12, 'bold'))
canvas1.create_window(150, 150, window=button1)

root.mainloop()
