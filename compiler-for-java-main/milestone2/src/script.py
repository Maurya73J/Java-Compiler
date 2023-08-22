import subprocess
import os
dir_path = input("Enter directory path:")
os.system("make clean")
os.system("mkdir ../output")
# print(cmd)
for path in reversed(os.listdir(dir_path)):
    os.system('make')
    os.system('echo "strict graph{ " > '+path+'.dot')
    os.system('./out < '+dir_path+'/'+ path + ' >> ' + path +'.dot')
    os.system('echo "}" >> '+path + '.dot')
    os.system('dot -Tpng ' + path + '.dot > ../output/'+ path+ '.png')

for path in reversed(os.listdir(dir_path)):
    os.system('make')
    os.system("mkdir ../output" + '/'+path)
    os.system('echo "strict graph{ " > ../output/'+path +'/'+path+'.dot')
    os.system('./out < '+dir_path+'/'+ path + ' >> ../output/'+path +'/' + path +'.dot')
    os.system('echo "}" >> ../output/'+path +'/' + path + '.dot')
    os.system('dot -Tpng ../output/'+path +'/' + path + '.dot > ../output/'+path +'/' + path + '.png')