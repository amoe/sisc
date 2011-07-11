package sisc.env;

import java.lang.reflect.*;
import java.beans.*;

import sisc.data.*;
import sisc.interpreter.*;
import java.io.IOException;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class NativeParameter extends Parameter {

    private Method readMethod;
    private Method writeMethod;
    
    private String fieldName;

    public NativeParameter() {}

    public NativeParameter(String fieldName) {
        this.fieldName = fieldName;
        init();
    }

    private void init() {
        try {
            PropertyDescriptor[] descriptors =
                Introspector.getBeanInfo(DynamicEnvironment.class).getPropertyDescriptors();
            for (int i=0; i<descriptors.length; i++) {
                PropertyDescriptor d = descriptors[i];
                if (d.getName().equals(fieldName)) {
                    readMethod = d.getReadMethod();
                    writeMethod = d.getWriteMethod();
                    return;
                }
            }
        } catch (IntrospectionException e) {}
        
        throw new RuntimeException(liMessage(SISCB, "nativeparamnotfound", fieldName));
    }

    public Value getValue(Interpreter r) throws ContinuationException {
        try {
            return (Value)readMethod.invoke(r.dynenv, new Object[]{});
        } catch (InvocationTargetException e) {
            error(r, liMessage(SISCB, "nativeparamaccess", e.getMessage()));
        } catch (IllegalAccessException e) {
            error(r, liMessage(SISCB, "nativeparamaccess", e.getMessage()));
        }
        return null;
    }

    public void setValue(Interpreter r, Value v) throws ContinuationException {
        try {
            writeMethod.invoke(r.dynenv, new Object[] {v});
        } catch (InvocationTargetException e) {
            error(r, liMessage(SISCB, "nativeparamaccess", e.getMessage()));
            throw new RuntimeException(e.getMessage());
        } catch (IllegalAccessException e) {
            error(r, liMessage(SISCB, "nativeparamaccess", e.getMessage()));
        }
    }

    public void display(ValueWriter w) throws IOException {
        w.append("#<").append(liMessage(SISCB, "nativeparameter"));
        w.append(" ").append(fieldName).append('>');
    }

    public void serialize(Serializer s) throws IOException {
        s.writeUTF(fieldName);
    }

    public void deserialize(Deserializer s) throws IOException {
        fieldName = s.readUTF();
        init();
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
