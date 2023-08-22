import subprocess
import os
os.system("make clean")
os.system("mkdir ../output")
dir_path = input("Enter directory path:")
# print(cmd)
for path in reversed(os.listdir(dir_path)):
    os.system("mkdir ../output" + '/'+path)
    os.system('make sym'+'<'+dir_path+'/'+ path)
    os.system('cp symbolTable.csv ../output/'+path+'/'+path+'_symbolTable.csv')
    ocm='make run'
    os.system(ocm)
    os.system('./out < '+dir_path+'/'+ path+' >> ../output/'+path+'/'+path+'.txt')
    os.system('cp gen.asm ../output/'+path+'/'+path+'_gen.asm')