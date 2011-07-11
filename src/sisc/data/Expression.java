package sisc.data;

import java.util.*;
import java.io.*;
import sisc.ser.*;
import sisc.exprs.fp.OptimismUnwarrantedException;
import sisc.interpreter.*;
import sisc.util.Util;
import sisc.util.ExpressionVisitor;
import sisc.util.ExpressionVisitee;

/**
 * The base class for any and all expressions.  An expression is anything 
 * in Scheme that can evaluate to a value.
 * 
 * In SISC, an expression can be <i>annotated</i>.  An annotation is 
 * any key/value pair attached to an expression for the duration of its 
 * lifetime.  Annotations are used to implement procedure properties 
 * and source-tracked debugging.
 */
public abstract class Expression extends Util 
    implements Externalizable, ExpressionVisitee {
    
    protected static Set EMPTYSET=new TreeSet();

    public Map annotations;
    
    public synchronized Value getAnnotation(Symbol key, Value def) {
        if (annotations==null)
            return def;
        Value res = (Value)annotations.get(key);
        return (res == null) ? def : res;
    }

    public Value getAnnotation(Symbol key) {
        return getAnnotation(key, FALSE);
    }

    public synchronized Value setAnnotation(Symbol key, Value val, Value def) {
        if (annotations==null)
            annotations=new HashMap(0);
        Value res = (Value)annotations.put(key, val);
        return (res == null) ? def : res;
    }

    public Value setAnnotation(Symbol key, Value val) {
        return setAnnotation(key, val, FALSE);
    }

    public synchronized Set getAnnotationKeys() {
        if (annotations==null) return EMPTYSET;
        return annotations.keySet();
    }

    public synchronized Pair getAnnotations() {
        Pair res = EMPTYLIST;
        if (annotations == null) return res;
        for (Iterator i = annotations.entrySet().iterator(); i.hasNext();) {
            Map.Entry en = (Map.Entry)i.next();
            res = new Pair(new Pair((Value)en.getKey(), (Value)en.getValue()),
                           res);
        }
        return res;
    }

    public void serializeAnnotations(Serializer s) throws IOException {
        if (annotations == null) {
            s.writeInt(0);
        } else {
            s.writeInt(annotations.size());
            for (Iterator i=annotations.entrySet().iterator(); i.hasNext();) {
                Map.Entry en = (Map.Entry)i.next();
                s.writeExpression((Expression)en.getKey());
                s.writeExpression((Expression)en.getValue());
            }
        }
    }

    public void deserializeAnnotations(Deserializer s) throws IOException {
        int ac = s.readInt();
        if (ac == 0) return;
        annotations = new HashMap(ac);
        for (; ac>0; ac--) {
            Expression key = s.readExpression();
            Expression val = s.readExpression();
            annotations.put(key, val);
        }
    }

    public boolean visitAnnotations(ExpressionVisitor v) {
        if (annotations == null) return true;
        for (Iterator i=annotations.entrySet().iterator(); i.hasNext();) {
            Map.Entry en = (Map.Entry)i.next();
            if (!v.visit((Expression)en.getKey())) return false;
            if (!v.visit((Expression)en.getValue())) return false;
        }
        return true;
    }

    /**
     * The following helpers set the 'name annotation, which is used for 
     * naming procedures, environments, etc.
     */
    public void setName(Symbol s) {
        setAnnotation(NAME, s);
    }

    public Symbol getName() {
        return (Symbol)getAnnotation(NAME, null);
    }

    /**
     * All expressions can be evaluated to some Scheme value.  This function 
     * implements the exact manner in which an expression is evaluated.  
     * An expression, when evaluated, performs some action that results 
     * in its value being placed in the <tt>acc</tt> register of the given
     * Interpreter.
     * 
     * An expression that immediately evaluates to a value (i.e. does not 
     * require that an additional expression be evaluated to obtain its 
     * value) <i>must</i> clear the <tt>nxp</tt> register by setting it to 
     * null.
     * Otherwise, <tt>nxp</tt> should be set to the expression that is to 
     * be evaluated next before this method returns.
     * 
     * @exception ContinuationException 
     * @param r the Interpreter
     */
    public abstract void eval(Interpreter r) throws ContinuationException;

    /**
     * If an expression implements the Immediate interface, it must 
     * override getValue, which returns as a Value, the immediate value 
     * of this expression. 
     * 
     * @exception ContinuationException 
     * @param r the Interpreter
     * @return the immediate value of this expression, or null if the value 
     *        cannot be obtained immediately.
     */
    public Value getValue(Interpreter r) throws ContinuationException, OptimismUnwarrantedException {
        return null;
    }

    /**
     * A debugging function, express returns a Scheme value that describes 
     * this expression.  See examples in sisc.exprs.*
     */
    public abstract Value express();

    /**
     * All Expressions must implement a default (no-argument) constructor.  
     * Those that wish to be serialized to a heap must implement both this 
     * and the deserialize method.  The Expression may use any method of 
     * the Serializer, which implements java.io.DataOutput
     * to write its state.
     * 
     * @exception IOException 
     * @param s the Serializer
     */
    public void serialize(Serializer s) throws IOException {}

    /**
     * All Expressions must implement a default (no-argument) constructor.  
     * Those that wish to b serialized to a heap must implement both this 
     * and the serialize method.  The Expression may use any method of the 
     * DataInput stream and the Deserializer serialization context
     * to read its state.
     * 
     * @exception IOException 
     * @param s the Deserializer
     */
    public void deserialize(Deserializer s) throws IOException {}

    public void writeExternal(ObjectOutput out) throws IOException {
        Serializer s = JavaSerializer.create(out);
        serialize(s);
        serializeAnnotations(s);
    }

    public void readExternal(ObjectInput in) throws IOException {
        Deserializer d = JavaDeserializer.create(in);
        deserialize(d);
        deserializeAnnotations(d);
    }

    public Object readResolve() throws ObjectStreamException {
        return (this instanceof Singleton) ?
            ((Singleton)this).singletonValue() : this;
    }

    public boolean visit(ExpressionVisitor v) {
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
