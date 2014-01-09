package sisc.ser;

import java.util.*;
import java.io.*;
import sisc.data.*;
import sisc.env.SymbolicEnvironment;
import sisc.interpreter.AppContext;
import sisc.util.InternedValue;

public abstract class SLL2Deserializer extends DeserializerImpl {

    private LinkedList deserQueue;

    protected SLL2Deserializer(AppContext ctx, ObjectInput input) throws IOException {
        super(ctx, input);
        deserQueue=new LinkedList();
    }

    public Expression readExpression() throws IOException {
        return readExpression(false, -1);
    }

    public Expression readInitializedExpression() throws IOException {
        return readExpression(true, -1);
    }

    protected abstract void recordReadObject(int definingOid, Expression e);

    protected abstract Expression skipReadObject(boolean flush, int definingOid)
        throws IOException;

    private Expression deserializeDetails(boolean flush,
                                          int definingOid,
                                          Expression e) throws IOException {
        if (e instanceof Singleton) {
            e.deserialize(this);
            e = ((Singleton)e).singletonValue();
            recordReadObject(definingOid, e);
        } else {
            recordReadObject(definingOid, e);
            int start=deserQueue.size();
            deserQueue.addFirst(e);
            if (flush) deserLoop(start);
        }
        return e;
    }

    protected Expression readExpression(boolean flush, int definingOid) throws IOException {
        int type=readInt();

        switch(type) {
          case 2: //shared expressions
              definingOid=readInt();
              return skipReadObject(flush, definingOid);
          case 3: //interned value
              Symbol name = (Symbol)readInitializedExpression();
              Class clazz = readClass();
              Expression e = InternedValue.deserResolve(name, clazz);
              return deserializeDetails(flush, definingOid, e);
          case 0: //ordinary expressions
              clazz=readClass();
              try {
                  e = (Expression)clazz.newInstance();
              } catch (InstantiationException ie) {
                  ie.printStackTrace();
                  throw new IOException(ie.getMessage());
              } catch (IllegalAccessException iae) {
                  iae.printStackTrace();
                  throw new IOException(iae.getMessage());
              }
              return deserializeDetails(flush, definingOid, e);
          case 1: //null
              return null;
          case 4:
              String libName=readUTF();
              int epid=readInt();
              e = resolveLibraryBinding(new LibraryBinding(libName, epid));
              recordReadObject(definingOid, e);
              return e;
          default: //expression references
              return fetchShared(type-16);
        }
    }

    public Expression deser() throws IOException {
        int start=deserQueue.size();
        Expression e=readExpression();
        deserLoop(start);
        return e;
    }
    
    private void deserLoop(int start) throws IOException {
        while (deserQueue.size()>start) {
            Object o=deserQueue.removeFirst();
            initializeExpression((Expression)o);
        }
    }

    private void initializeExpression(Expression e) throws IOException {
        e.deserialize(this);
        e.deserializeAnnotations(this);
    }
    
    abstract protected Expression fetchShared(int oid) throws IOException;
    
    public SymbolicEnvironment readSymbolicEnvironment() throws IOException {
        Expression e=readExpression();
        if (e instanceof Symbol) 
            e=ctx.getExpression((Symbol)e);
        return (SymbolicEnvironment)e;
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
