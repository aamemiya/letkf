import struct 
fid=open('smp.dat','rb')
#for i in range(4):
#    data=fid.read(4)
data=fid.read()
a=struct.unpack('f'*6,data)
print(a) 
