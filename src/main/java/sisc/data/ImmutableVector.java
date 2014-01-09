package sisc.data;

import java.io.IOException;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class ImmutableVector extends SchemeVector {

    private boolean isImmutable = true;

    public ImmutableVector() {}

    public ImmutableVector(int count) {
        super(count);
    }

    public ImmutableVector(int count, boolean isImmutable) {
        super(count);
        this.isImmutable = isImmutable;
    }

    public ImmutableVector(int count, Value initializer) {
        super(count, initializer);
    }

    public ImmutableVector(Value[] v) {
        super(v);
    }

    public void set(int idx, Value v) {
        if (isImmutable) throw new RuntimeException(liMessage(SISCB, "vectorisimmutable"));
        else super.set(idx, v);
    }

    public void fill(Value v) {
        if (isImmutable) throw new RuntimeException(liMessage(SISCB, "vectorisimmutable"));
        else super.fill(v);
    }

    public boolean isImmutable() {
        return isImmutable;
    }

    public void makeImmutable() {
        isImmutable = true;
    }

    public void serialize(Serializer s) throws IOException {
        super.serialize(s);
        s.writeBoolean(isImmutable);
    }

    public void deserialize(Deserializer s) throws IOException {
        super.deserialize(s);
        isImmutable = s.readBoolean();
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
