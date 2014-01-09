package sisc.ser;

import java.io.*;

public class BufferedRandomAccessInputStream extends SeekableInputStream {

    protected RandomAccessFile raf;
    protected int stackDepth, bufferSize;
    protected byte[][] buffer;
    protected int dirtyRange[][], eofAt[];
    protected long offset[];
    protected int bufferPtr;

    public BufferedRandomAccessInputStream(String name, String mode) 
        throws IOException {

        this(name, mode, 4096);
    }

    public BufferedRandomAccessInputStream(String name, String mode, int bufferSize) 
        throws IOException {

        this(name, mode, 3, bufferSize);
    }

    public BufferedRandomAccessInputStream(String name, String mode, 
                                    int stackDepth, int bufferSize) 
        throws IOException {

        this(new File(name), mode, stackDepth, bufferSize);
    }

    public BufferedRandomAccessInputStream(File f, String mode, int stackDepth,
                                           int bufferSize)
        throws IOException {

        raf=new RandomAccessFile(f, mode);
        this.stackDepth=stackDepth;
        this.bufferSize=bufferSize;
        buffer=new byte[stackDepth][bufferSize];
        dirtyRange=new int[stackDepth][2];
        offset=new long[stackDepth];
        eofAt=new int[stackDepth];
        for (int i=0; i<stackDepth; i++) {
            dirtyRange[i][0]=-1;
            offset[i]=-1;
            eofAt[i]=-1;
        }
        raf.seek(0);
        acquire(0);
    }

    protected void flush(int bufferNumber) throws IOException {
        if (dirtyRange[bufferNumber][0]>-1) {
            raf.seek(offset[bufferNumber]+dirtyRange[bufferNumber][0]);
            raf.write(buffer[bufferNumber], 
                        dirtyRange[bufferNumber][0], 
                        dirtyRange[bufferNumber][1]-dirtyRange[bufferNumber][0]);
            dirtyRange[bufferNumber][0]=-1;
            dirtyRange[bufferNumber][0]=0;
        }
    }

    public void flush() throws IOException {
        for (int i=0; i<stackDepth; i++) {
            if (dirtyRange[i][0]>-1) {
                flush(i);
            }
        }
    }

    /**
     * Set an existing buffer as the active buffer and set the 
     * buffer pointer
     */
    protected void activate(int bufferNumber, int off) {
        if (bufferNumber!=0) {
            int[] dirtyRangeTmp=dirtyRange[bufferNumber];
            byte[] bufferTmp=buffer[bufferNumber];
            long offsetTmp=offset[bufferNumber];
            int eofAtTmp=eofAt[bufferNumber];
            for (int i=bufferNumber; i>0; i--) {
                dirtyRange[i]=dirtyRange[i-1];
                buffer[i]=buffer[i-1];
                offset[i]=offset[i-1];
                eofAt[i]=eofAt[i-1];
            }
            
            buffer[0]=bufferTmp;
            offset[0]=offsetTmp;
            eofAt[0]=eofAtTmp;
            dirtyRange[0]=dirtyRangeTmp;
        }
        bufferPtr=off;
    }

    protected void load(long pos) throws IOException {
        //Where do we really want to be?  If we can 'cozy up' to 
        //an existing buffer, we'll probably save ourselves some reads
        //in the future
        //System.err.println("seekTo:"+pos);
        long backReach=pos-bufferSize;
        long forwardReach=pos;

        long seekPos=pos;

        for (int i=stackDepth-2; i>=0; i--) {
            if (offset[i]!=-1 &&
                backReach>offset[i] &&
                backReach<(offset[i]+bufferSize)) {
                //System.err.println("Br");
                //There is a buffer that is going to run into us, 
                //lets seek there
                seekPos=offset[i]+bufferSize;
                break;
            }
        }

        for (int i=stackDepth-2; i>=0; i--) {
            if (offset[i]!=-1 &&
                forwardReach>=offset[i] &&
                forwardReach<(offset[i]+bufferSize)) {
                //System.err.println("Fr");
                //There is a buffer that we'll run into, lets 
                //make sure that happens
                //lets seek there
                seekPos=offset[i]-bufferSize;
                break;
            }
        }
        //System.err.println(pos+":"+seekPos);
            
        flush(stackDepth-1);
        
        int[] dirtyRangeTmp=dirtyRange[stackDepth - 1];
        byte[] bufferTmp=buffer[stackDepth - 1];

        for (int i=stackDepth-1; i>0; i--) {
            dirtyRange[i]=dirtyRange[i-1];
            buffer[i]=buffer[i-1];
            offset[i]=offset[i-1];
            eofAt[i]=eofAt[i-1];
        }

        dirtyRange[0]=dirtyRangeTmp;
        buffer[0]=bufferTmp;

        dirtyRange[0][0]=-1;
        dirtyRange[0][1]=0;
        offset[0]=-1;
        raf.seek(seekPos);
        advance();
        bufferPtr=(int)(pos-seekPos);
    }

    protected final void advance() throws IOException {
        flush(0);
        long cp=raf.getFilePointer();
        if (offset[0]==-1) offset[0]=cp;
        else { 
            offset[0]+=bufferSize;
            if (cp!=offset[0]) {
                raf.seek(offset[0]);
            }
        }

        int br=0;
        int rv=raf.read(buffer[0], 0, bufferSize);
        while (rv>-1 && br<bufferSize) {
            br+=rv;
            rv=raf.read(buffer[0], br, bufferSize-br);
        }
        if (br<bufferSize) eofAt[0]=br;
        else {
            eofAt[0]=-1;
        }
        bufferPtr=0;
    }

    protected void acquire(long pos) throws IOException {
        for (int i=0; i<stackDepth; i++) {
            if (offset[i]!=-1 &&
                pos >= offset[i] && 
                pos < (offset[i]+bufferSize)) {
                activate(i, (int)(pos-offset[i]));
                return;
            }
        }
        //None of the buffers can handle this position, get the new one.
        load(pos);
    }

    protected final void advancePointer(int n) throws IOException {
        bufferPtr+=n;
        if (bufferPtr >= bufferSize) 
            advance();
    }

    protected final void advancePointerWrite(int n) throws IOException {
        if (dirtyRange[0][0]==-1) 
            dirtyRange[0][0]=bufferPtr;
        bufferPtr+=n;
        dirtyRange[0][1]+=n;
        if (bufferPtr >= bufferSize) 
            advance();
    }
            
            
    
    //---------------//
    public void close() throws IOException {
        flush();
        raf.close();
    }

    public int read() throws IOException {
        if (bufferPtr == eofAt[0])
            return -1;
        int rv=buffer[0][bufferPtr] & 0xff;
        advancePointer(1);
        return rv;
    }

    public int read(byte[] b, int off, int len) throws IOException {
        int bc=Math.min(len, Math.min(bufferSize-bufferPtr, 
                                      (eofAt[0] == -1 ? 
                                       bufferSize : 
                                       eofAt[0]-bufferPtr)));

        if (bc==0 && len!=0) 
            return -1;
        System.arraycopy(buffer[0], bufferPtr, b, off, bc);
        advancePointer(bc);
        return bc;
    }

    public int read(byte[] b) throws IOException {
        return read(b, 0, b.length);
    }


    public int skipBytes(int n) throws IOException {
        int bc=bufferSize - bufferPtr;
        advancePointer(bc);
        return bc;
    }

    public void write(int b) throws IOException {
        buffer[0][bufferPtr]=(byte)b;
        advancePointerWrite(1);
    }
    
    public void write(byte[] b, int off, int len) throws IOException {
        do {
            int bc=Math.min(len, bufferSize - bufferPtr);
            len-=bc;
            System.arraycopy(b, off, buffer[0], bufferPtr, bc);
            advancePointerWrite(bc);
        } while (len>0);
    }

    public void write(byte[] b) throws IOException {
        write(b, 0, b.length);
    }

    public long getFilePointer() throws IOException {
        return offset[0]+bufferPtr;
    }

    public void seek(long pos) throws IOException {
        acquire(pos);
    }

    //Test
    public static void main(String[] args) throws IOException {
        BufferedRandomAccessInputStream is=new BufferedRandomAccessInputStream(args[0], "r",4);
        int rv;
        do { 
            rv=is.read();
            if (is.getFilePointer()==10)
                is.seek(13);
            if (is.getFilePointer()==18)
                is.seek(12);
        } while (rv!=-1);
    }
}

/*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 * 
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 * 
 * The Original Code is the Second Interpreter of Scheme Code (SISC).
 * 
 * The Initial Developer of the Original Code is Scott G. Miller.
 * Portions created by Scott G. Miller are Copyright (C) 2000-2007
 * Scott G. Miller.  All Rights Reserved.
 * 
 * Contributor(s):
 * Matthias Radestock 
 * 
 * Alternatively, the contents of this file may be used under the
 * terms of the GNU General Public License Version 2 or later (the
 * "GPL"), in which case the provisions of the GPL are applicable 
 * instead of those above.  If you wish to allow use of your 
 * version of this file only under the terms of the GPL and not to
 * allow others to use your version of this file under the MPL,
 * indicate your decision by deleting the provisions above and
 * replace them with the notice and other provisions required by
 * the GPL.  If you do not delete the provisions above, a recipient
 * may use your version of this file under either the MPL or the
 * GPL.
 */
