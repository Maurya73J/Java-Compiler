import subprocess
import os
i=1
dir_path = input("Enter directory path:")
# print(cmd)
for path in reversed(os.listdir(dir_path)):
    os.system('make sym'+'<'+dir_path+'/'+ path)
    ocm='make run < '+dir_path+'/'+ path+' >> '+str(i)+'.txt'
    os.system(ocm)
    i=i+1