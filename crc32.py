from __future__ import print_function
import sys,os,os.path,glob,zlib

def main():
    folder=sys.argv[1] if len(sys.argv)>=2 else "."
    names=os.listdir(folder)

    print(' CRC 32     File Size   Filename')
    print('-------------------------------------------------')
    
    for name in names:
        full_name=os.path.join(folder,name)
        if not os.path.isfile(full_name): continue
        with open(full_name,'rb') as f: data=f.read()
        print ('%08x  %9d  %s'%(zlib.crc32(data)&0xffffffff,
                                len(data),
                                full_name))

if __name__=='__main__': main()
