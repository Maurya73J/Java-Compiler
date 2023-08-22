import subprocess
import os
dir_path = input("Enter directory path:")
os.system("mkdir ../output")
# print(cmd)
for path in reversed(os.listdir(dir_path)):
    os.system("mkdir ../output" + '/'+path)
    os.system('make sym'+'<'+dir_path+'/'+ path)
    os.system('cp symbolTable.csv ../output/'+path+'/'+path+'_symbolTable.csv')
    os.system('make run')
    ocm='./out < '+dir_path+'/'+ path+' >> ../output/'+path+'/'+path+'.txt'
    os.system(ocm)
