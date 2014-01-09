package sisc.ser;

import java.util.*;
import java.io.*;
import sisc.data.Expression;
import sisc.interpreter.AppContext;

public class StreamSerializer extends SLL2Serializer {

    private Map entryPoints, classes;
    private int nextEp, nextClassIdx;

    private StreamSerializer(AppContext ctx, NestedObjectOutputStream out) throws IOException {
        super(ctx, out);
        out.setSerializerInstance(this);
        this.classes=new HashMap();
        this.entryPoints=new HashMap();
    }
    public StreamSerializer(AppContext ctx, OutputStream out) throws IOException {
        this(ctx, new NestedObjectOutputStream(out));
    }
    
    protected void writeExpression(Expression e, boolean flush)
        throws IOException {

        int posi=nextEp;
        
        Integer epIndex=(Integer)entryPoints.get(e);
        if (epIndex != null) {
            writeSeenEntryPoint(epIndex.intValue());
        }  else {
            entryPoints.put(e, new Integer(nextEp));
            writeNewEntryPointMarker(nextEp, e);
            nextEp++;
            writeExpression(e, posi, -1, flush);
        }
    }
    
    public void writeClass(Class c) throws IOException {
        Integer classIdx=(Integer)classes.get(c);
        if (classIdx == null) {
            classes.put(c, new Integer(nextClassIdx));
            writeInt(nextClassIdx);
            nextClassIdx++;
            writeUTF(c.getName());
        } else {
            writeInt(classIdx.intValue());
        }
    }

    protected void serializeEnd(int posi, int sizeStartOffset) {
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
