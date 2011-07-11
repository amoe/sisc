package sisc.ser;

import sisc.util.Util;
import java.util.*;
import java.io.*;
import sisc.data.*;
import sisc.interpreter.AppContext;

public class StreamDeserializer extends SLL2Deserializer {

    private Map classPool, alreadyReadObjects;

    private StreamDeserializer(AppContext ctx, NestedObjectInputStream input) throws IOException {
        super(ctx, input);
        input.setDeserializerInstance(this);
        classPool=new HashMap();
        alreadyReadObjects=new HashMap();
    }

    public StreamDeserializer(AppContext ctx, InputStream input) throws IOException {
        this(ctx, new NestedObjectInputStream(input));
    }

    protected void recordReadObject(int definingOid, Expression e) {
        if (definingOid!=-1) {
            Integer epIdx=new Integer(definingOid);
            if (alreadyReadObjects.get(epIdx)==null)
                alreadyReadObjects.put(epIdx, e);
        }
    }

    protected Expression skipReadObject(boolean flush, int definingOid)
        throws IOException {

        Integer epIdx=new Integer(definingOid);
        Expression e = (Expression)alreadyReadObjects.get(epIdx);
        if (e == null) {
            return readExpression(flush, definingOid);
        } else {
            //we should never really get here
            readExpression(flush, definingOid);
            return e;
        }
    }

    protected Expression fetchShared(int oid) throws IOException {
        try {
            Expression e=(Expression)alreadyReadObjects.get(new Integer(oid));
            if (e==null) {
                throw new IOException(Util.liMessage(Util.SISCB, "undefedepinstream"));
            }
            return e;
        } catch (ArrayIndexOutOfBoundsException aib) {
            throw new FileNotFoundException(Util.liMessage(Util.SISCB, "invalidentrypoint", new Object[] {new Integer(oid)}));
        }
    }            
    
    public Class readClass() throws IOException {
        int cid=readInt();
        Integer i=new Integer(cid);
        Class c=(Class)classPool.get(i);
        if (c==null) {
            try {
                c=Class.forName(readUTF(), true, Util.currentClassLoader());
                classPool.put(i, c);
            } catch (ClassNotFoundException cnf) {
                throw new IOException(cnf.toString());
            }
        }
        return c;
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
