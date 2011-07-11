package sisc.ser;

import sisc.util.Util;
import java.util.*;
import java.io.*;
import sisc.data.*;
import sisc.interpreter.AppContext;

public class BlockDeserializer extends SLL2Deserializer implements LibraryDeserializer {

    private Map classPool;
    Expression[] alreadyReadObjects;
    private int[] offsets, sizes;
    private long base;
    private Library baseLib;
    
    public BlockDeserializer(AppContext ctx, SeekableDataInputStream input, 
                             Map classes, int[] o, int[] l) throws IOException {
        super(ctx, input);
        base=input.getFilePointer();
        classPool=classes;
        
        offsets=o;
        sizes=l;
        alreadyReadObjects=new Expression[offsets.length];
    }
    
    protected void recordReadObject(int definingOid, Expression e) {
        if (definingOid!=-1 && alreadyReadObjects[definingOid]==null)
            alreadyReadObjects[definingOid]=e;
    }

    protected Expression skipReadObject(boolean flush, int definingOid)
        throws IOException {

        Expression e = (Expression)alreadyReadObjects[definingOid];
        if (e == null) {
            return readExpression(true, definingOid);
        } else {
            int sc=sizes[definingOid];
            while (sc>0) {
                sc-=datin.skipBytes(sc);
            }
            return e;
        }
    }

    protected Expression fetchShared(int oid) throws IOException {
        try {
            Expression e=alreadyReadObjects[oid];
            if (e==null) {
                long currentPos=((Seekable)datin).getFilePointer();
                ((Seekable)datin).seek(base + offsets[oid]);
                e=deser();
                ((Seekable)datin).seek(currentPos);
            }
            return e;
        } catch (ArrayIndexOutOfBoundsException aib) {
            throw new FileNotFoundException(Util.liMessage(Util.SISCB, "invalidentrypoint", new Object[] {new Integer(oid)}));
        }
    }            

    public Library getLibrary() {
        return baseLib;
    }

    public void setLibrary(Library lib) {
        this.baseLib = lib;
    }

    public Class readClass() throws IOException {
        return (Class)classPool.get(new Integer(readInt()));
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
