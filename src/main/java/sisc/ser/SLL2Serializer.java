package sisc.ser;

import java.io.*;
import java.util.*;
import sisc.data.Expression;
import sisc.data.Singleton;
import sisc.data.Value;
import sisc.interpreter.AppContext;
import sisc.util.InternedValue;

public abstract class SLL2Serializer extends SerializerImpl {

    private static class SerJobEnd {
        public int posi;
        public int sizeStartOffset;
        
        public SerJobEnd(int posi, int sizeStartOffset) {
            this.posi=posi;
            this.sizeStartOffset=sizeStartOffset;
        }
    }

    private LinkedList serQueue;
    
    protected SLL2Serializer(AppContext ctx, ObjectOutput out) throws IOException {
        super(ctx, out);
        serQueue=new LinkedList();
    }

    /**
     * Required call which actually writes out the bytes of an expression
     * 
     * @param e
     * @param flush
     * @throws IOException
     */     
    protected abstract void writeExpression(Expression e, boolean flush) throws IOException;
   
    protected abstract void serializeEnd(int posi, int sizeStartOffset);

    public void writeExpression(Expression e) throws IOException {
        writeExpressionHelper(e, false);
    }

    public void writeInitializedExpression(Expression e) throws IOException {
        writeExpressionHelper(e, true);
    }

    /**
     * Serializes expressions. We distinguish betweeen six types of
     * expressions:
     * Type 0: normal expression
     * Type 1: null
     * Type 2: first encounter of entry point / shared expression
     * Type 3: interned value
     * Type 4: entry point into other library
     * Type 16+n: reference to entry point / shared expression n
     *
     * @param e the expression to serialize
     * @param flush force complete, immediate serialisation
     * @exception IOException if an error occurs
     */
    private void writeExpressionHelper(Expression e, boolean flush)
        throws IOException {

        if (e == null) {
            writeInt(1);
            return;
        }
        writeExpression(e, flush);
    }

    public void serialize(Expression e) throws IOException {
        int start=serQueue.size();
        writeExpression(e);
        serLoop(start);
    }

    protected boolean writeExpression(Expression e, int pos, int offset,
                                      boolean flush) throws IOException {
        SerJobEnd job = new SerJobEnd(pos, offset);
        boolean contiguous;
        if (e instanceof Singleton) {
            contiguous = writeExpressionSerialization(e, job, flush);
        } else {
            LibraryBinding lb = lookupLibraryBinding(e);
            contiguous = (lb==null) ?
                writeExpressionSerialization(e, job, flush) :
                writeLibraryReference(lb, job, flush);
        }
        return contiguous;
    }

    private void serLoop(int start) throws IOException {
        while (serQueue.size()>start) {
            Object o=serQueue.removeFirst();
            if (o instanceof Expression) {
                serializeDetails((Expression)o);
            } else if (o instanceof SerJobEnd) {
                SerJobEnd job = (SerJobEnd)o;
                serializeEnd(job.posi, job.sizeStartOffset);
            } 
        }
    }

    private void serializeDetails(Expression e) throws IOException {
        e.serialize(this);
        e.serializeAnnotations(this);
    }

    public void close() throws IOException {
        flush();
        super.close();
    }

    public void flush() throws IOException {
        serLoop(0);
        super.flush();
    }

    protected void writeSeenEntryPoint(int posi) throws IOException {
        writeInt(posi+16);
    }

    protected void writeNewEntryPointMarker(int posi, Expression e) throws IOException {
        writeInt(2);
        writeInt(posi);
    }
    
    private boolean writeExpressionSerialization(Expression e, SerJobEnd end, boolean flush) throws IOException {
        if (e instanceof Value) {
            InternedValue iv = InternedValue.lookupByValue((Value)e);
            if (iv == null) {
                writeInt(0);
            } else {
                writeInt(3);
                writeInitializedExpression(iv.getName());
            }
        } else {
            writeInt(0);
        }
        writeClass(e.getClass());
        if (e instanceof Singleton) {
            e.serialize(this);
            return true;
        } else {
            int start=serQueue.size();
            serQueue.addFirst(end);
            serQueue.addFirst(e);
            if (flush)
                serLoop(start);
            return false;
        }
    }

    private boolean writeLibraryReference(LibraryBinding lb, SerJobEnd end, boolean flush) throws IOException {
        writeInt(4);
        writeUTF(lb.name);
        writeInt(lb.epid);
        return true;
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
