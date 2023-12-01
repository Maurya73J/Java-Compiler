import subprocess
import os

dir_path = input("Enter directory path:")
i=1
for path in os.listdir(dir_path):
    cmd='make < '+dir_path+'/'+ path
    os.system(cmd)
    #output file command with i
    i=i+1
    os.system('make clean')
