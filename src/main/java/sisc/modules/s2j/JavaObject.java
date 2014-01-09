package sisc.modules.s2j;

import java.lang.reflect.*;

import sisc.data.*;

import java.io.IOException;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.data.Expression;
import sisc.util.ExpressionVisitor;

public class JavaObject extends Value {

    protected Object obj;

    public static final byte
        JUNKN   = 0,
        JNULL   = 1,
        JCLASS  = 2,
        JFIELD  = 3,
        JMETHOD = 4,
        JCONSTR = 5,
        JARRAY  = 6,
        JOBJ    = 7;

    protected byte objType = JUNKN;

    public byte getObjType() {
        //we do not need to make this synchronized since we are only
        //accessing a byte
        if (objType == JUNKN)
            objType = 
                (obj instanceof Class ? JCLASS :
                 (obj instanceof Field ? JFIELD :
                  (obj instanceof Method ? JMETHOD :
                   (obj instanceof Constructor ? JCONSTR :
                    (obj.getClass().isArray() ? JARRAY :
                     JOBJ)))));
        return objType;
    }

    public JavaObject() {}

    public void serialize(Serializer s) throws IOException {
        byte ty = getObjType();
        s.writeByte(ty);
        switch (ty) {
        case JCLASS: {
            s.writeUTF(Util.nameType((Class)obj));
            break;
        }
        case JFIELD: {
            Field f = (Field)obj;
            s.writeUTF(Util.nameType(f.getDeclaringClass()));
            s.writeUTF(f.getName());
            break;
        }
        case JMETHOD: {
            Method m = (Method)obj;
            s.writeUTF(Util.nameType(m.getDeclaringClass()));
            s.writeUTF(m.getName());
            Class[] types = m.getParameterTypes();
            s.writeInt(types.length);
            for (int i=0; i < types.length; i++) {
                s.writeUTF(Util.nameType(types[i]));
            }
            break;
        }
        case JCONSTR: {
            Constructor c = (Constructor)obj;
            s.writeUTF(Util.nameType(c.getDeclaringClass()));
            Class[] types = c.getParameterTypes();
            s.writeInt(types.length);
            for (int i=0; i < types.length; i++) {
                s.writeUTF(Util.nameType(types[i]));
            }
            break;
        }
        default:
            s.writeObject(obj);
        }
    }

    public void deserialize(Deserializer s) throws IOException {
        byte ty = s.readByte();
        switch (ty) {
        case JCLASS: {
            obj = Util.resolveType(s.readUTF());
            break;
        }
        case JFIELD:
            try {
                Class c = Util.resolveType(s.readUTF());
                obj = c.getDeclaredField(s.readUTF());
            } catch (NoSuchFieldException e) {
                throw new RuntimeException(liMessage(Util.S2JB, "cannotdeserialize"));
            }
            break;
        case JMETHOD:
            try {
                Class c = Util.resolveType(s.readUTF());
                String n = s.readUTF();
                int l = s.readInt();
                Class types[] = new Class[l];
                for (int i=0; i < l; i++) {
                    types[i] = Util.resolveType(s.readUTF());
                }
                obj = c.getDeclaredMethod(n, types);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException(liMessage(Util.S2JB, "cannotdeserialize"));
            }
            break;
        case JCONSTR:
            try {
                Class c = Util.resolveType(s.readUTF());
                int l = s.readInt();
                Class types[] = new Class[l];
                for (int i=0; i < l; i++) {
                    types[i] = Util.resolveType(s.readUTF());
                }
                obj = c.getDeclaredConstructor(types);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException(liMessage(Util.S2JB, "cannotdeserialize"));
            }
            break;
        default:
            try {
                obj = s.readObject();
            } catch (ClassNotFoundException e) {
                throw new RuntimeException(liMessage(Util.S2JB, "cannotdeserialize"));
            }
            break;
        }
    }

    public boolean visit(ExpressionVisitor v) {
        return (obj != null && obj instanceof Expression) ?
            v.visit((Expression)obj) : true;
    }

    public JavaObject(Object o) {
        this.obj = o;
    }

    public Object get() {
        return obj;
    }

    public Class classOf() {
        return obj.getClass();
    }

    public void display(ValueWriter w) throws IOException {
        w.append("#<java ")
            .append(Util.nameType(obj.getClass()))
            .append(' ')
            .append((getObjType() == JCLASS) ? Util.nameType((Class)obj) : obj.toString())
            .append('>');
    }

    public int hashCode() {
        return obj.hashCode();
    }

    public boolean eqv(Object v) {
        return this == v || (v != null && v.getClass() == JavaObject.class
                             && obj == ((JavaObject)v).obj);
    }

    public boolean valueEqual(Value v) {
        return eqv(v) || (v instanceof JavaObject
                         && obj.equals(((JavaObject)v).get()));
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
