package sisc.ser;

import java.util.*;
import java.io.*;
import sisc.data.Expression;
import sisc.interpreter.AppContext;

public class BlockSerializer extends SLL2Serializer {

    private static class CountingOutputStream extends FilterOutputStream {

        public int position;

        public CountingOutputStream(OutputStream out) {
            super(out);
            position=0;
        }

        public void write(int b) throws IOException {
            position++;
            out.write(b);
        }

        public void write(byte[] b, int offset, int len) throws IOException {
            position+=len;
            out.write(b, offset, len);
        }
    }

    private static class WrappedDataOutputStream extends DataOutputStream implements ObjectOutput {

        public WrappedDataOutputStream(OutputStream out) {
            super(out);
        }

        public void writeObject(Object o) throws IOException {
            throw new IOException("cannot serialize " + o);
        }

    }

    private CountingOutputStream cos;
    private Map offsets;
    private Set seen;
    private Vector classes;
    private int[] sizes;
    private Expression[] entryPoints;
    private HashMap epi, ci;

    private BlockSerializer(AppContext ctx, CountingOutputStream cos,
                            Vector classes, Expression[] entryPoints)
        throws IOException {

        super(ctx, new WrappedDataOutputStream(cos));
        this.cos = cos;
        this.classes=classes;
        this.entryPoints=entryPoints;
        sizes=new int[entryPoints.length];
        offsets=new HashMap();
        seen=new HashSet();

        ci=new HashMap();
        for (int i=0; i<classes.size(); i++) {
            ci.put(classes.elementAt(i), new Integer(i));
        }

        epi=new HashMap();
        for (int i=0; i<entryPoints.length; i++) {
            epi.put(entryPoints[i], new Integer(i));
        }
    }

    public BlockSerializer(AppContext ctx, OutputStream out, Vector classes, 
                           Expression[] entryPoints) throws IOException {
        this(ctx, new CountingOutputStream(out), classes, entryPoints);
    }

    public int[] getSizes() {
        return sizes;
    }

    protected void writeExpression(Expression e, boolean flush)
        throws IOException {

        Integer epIndex=(Integer)epi.get(e);

        int sizeStartOffset = -1;
        int posi=-1;

        if (epIndex!=null) {
            posi=epIndex.intValue();
            //entry point / shared expression
            if (seen.contains(e)) {
                writeSeenEntryPoint(posi);
                return;
            }  else {
                seen.add(e);
                offsets.put(new Integer(posi), new Integer(cos.position));
                writeNewEntryPointMarker(posi, e);
                sizeStartOffset = cos.position;
                flush=true;
            }
        } 
        boolean contiguous = writeExpression(e, posi, sizeStartOffset, flush);
        if (contiguous) {
            //If true, the item was serialized contiguously, so we can
            //record its size now
            if (sizeStartOffset != -1)
                sizes[posi] = cos.position - sizeStartOffset;
        } // else we'll get it in a serializeEnd() callback
    }

    public void writeClass(Class c) throws IOException {
        writeInt(((Integer)ci.get(c)).intValue());
    }

    protected void serializeEnd(int posi, int sizeStartOffset) {
        if (sizeStartOffset != -1)
            sizes[posi] = cos.position - sizeStartOffset;
    }

    public int[] getOffsets() {
        int[] i=new int[offsets.size()];
        for (Iterator x=offsets.keySet().iterator(); x.hasNext();) {
            Integer key=(Integer)x.next();
            i[key.intValue()]=((Integer)offsets.get(key)).intValue();
        }
        return i;
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
