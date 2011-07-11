package sisc.interpreter;

import java.io.*;
import sisc.data.*;
import sisc.env.SymbolicEnvironment;
import sisc.io.ValueWriter;
import sisc.util.ExpressionVisitor;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;

public class CallFrame extends Procedure {

    public Expression            nxp;
    public Value[]               vlr, lcl, env;
    public boolean               vlk; //indicates that this frame has
                                      //been captured by a continuation
    public CallFrame             fk, parent;
    public SymbolicEnvironment   tpl; //The currently active top-level environment
    
    public StackTracer           tracer;

    public CallFrame(Expression n, Value[] v,
                     boolean vk, Value[] l, Value[] e,
                     SymbolicEnvironment t,
                     CallFrame f, CallFrame p,
                     StackTracer tr) {
        /*
          We really just want to call init(n,v,vk,l,e,t,f,p) here but
          that seems lose us 2% on gabriel benchmarks, at least on
          Sun's 1.5.0_05-b05 JVM on Linx 2.6.8-1-686-smp.
        */
        nxp=n;
        vlr=v;
        vlk=vk;
        lcl=l;
        env=e;
        tpl=t;
        fk=f;
        parent=p;
        tracer=tr;
    }

    public final void init(Expression n, Value[] v,
                           boolean vk, Value[] l, Value[] e,
                           SymbolicEnvironment t,
                           CallFrame f, CallFrame p,
                           StackTracer tr) {
        nxp=n;
        vlr=v;
        vlk=vk;
        lcl=l;
        env=e;
        tpl=t;
        fk=f;
        parent=p;
        tracer=tr;
    }

    public final void clear() {
        vlr=lcl=env=null;
        fk=parent=null;
        tpl=null;
        tracer=null;
    }

    public final CallFrame capture(Interpreter r) {
        // Set the captured flags all the way to the root.
        for (CallFrame w=this; w!=null && !w.vlk; w=w.parent) {
            w.vlk=true;
        }

        return this;
    }
    
    public void display(ValueWriter w) throws IOException {    
        displayNamedOpaque(w, "continuation"); 
    }

    public static boolean visitValueArray(ExpressionVisitor v, Value[] va) {
        if (va==null) return true;
        for (int i=0; i<va.length; i++) 
            if (!v.visit(va[i])) return false;
        return true;
    }

    public boolean visit(ExpressionVisitor v) {
        return visitValueArray(v, vlr) && visitValueArray(v,lcl) && 
            visitValueArray(v,env) && v.visit(tpl) && v.visit(nxp) && v.visit(fk) && 
            v.visit(parent) && (tracer == null || v.visit(tracer));
    }

    public void apply(Interpreter r) throws ContinuationException {
        if (r.vlr.length==1) 
            r.acc=r.vlr[0];
        else 
            r.acc=new Values(r.vlr);

        r.pop(this);
    }

    public Value express() {
        return list(sym("frame"),
                    list(truth(vlk),
                         valArrayToVec(vlr),
                         valArrayToVec(lcl),
                         valArrayToVec(env)),
                    (parent == null ? (Value)FALSE : parent),
                    (fk == null ? (Value)FALSE : fk),
                    (nxp == null ? VOID : nxp.express()));
    }

    public void serialize(Serializer s) throws IOException {
        s.writeBoolean(vlk);
        if (vlr==null)
            s.writeBoolean(false);
        else {
            s.writeBoolean(true);
            s.writeExpressionArray(vlr);
        }
        s.writeExpressionArray(lcl);
        s.writeExpressionArray(env);
        s.writeSymbolicEnvironment(tpl);
        s.writeExpression(nxp);
        s.writeExpression(fk);
        s.writeExpression(parent);
        s.writeBoolean(tracer!=null);
        if (tracer != null) {
        	tracer.serialize(s);
        }
    }

    public CallFrame() {}

    public void deserialize(Deserializer s) throws IOException {
        vlk=s.readBoolean();
        vlr=null;
        if (s.readBoolean()) {
            vlr=s.readValueArray();
        }
        lcl=s.readValueArray();
        env=s.readValueArray();
        tpl=s.readSymbolicEnvironment();
        nxp=s.readExpression();
        fk=(CallFrame)s.readExpression();
        parent=(CallFrame)s.readExpression();
        if (s.readBoolean()) {
        	tracer=new StackTracer();
        	tracer.deserialize(s);
        }
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
