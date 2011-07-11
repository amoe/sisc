package sisc.data;

import java.lang.ref.WeakReference;

import sisc.env.*;
import sisc.interpreter.*;
import sisc.io.ValueWriter;

import java.io.IOException;

public class SchemeThread extends Value implements Runnable, NamedValue {
    public static final int READY=0, RUNNING=1, FINISHED=2, FINISHED_ABNORMALLY=3;

    public static final ThreadGroup 
        schemeThreads=new ThreadGroup("SISC Threads");

    public DynamicEnvironment env;
    public Procedure thunk;
    public Thread thread;
    public ThreadContext threadContext;
    public int state;
    public Value rv;

    public SchemeThread(DynamicEnvironment dynenv, Procedure thunk) {
        this.env = dynenv.copy();
        this.env.wind = FALSE;
        this.thunk = thunk;
        thread=new Thread(schemeThreads, this);
        state=READY;

    }
    
    public Value getResult(Interpreter r) throws ContinuationException {
        if (state < FINISHED) {
            throw new RuntimeException(liMessage(SISCB,"threadnotyetterminated"));
        } else if (state == FINISHED) {
            return rv;
        } else {
            Values v=(Values)rv;
            Procedure.throwNestedPrimException(new SchemeException(pair(v.values[0]), 
                                                                   proc(v.values[1]),     
                                                                   v.values.length>2 ? 
                                                                   proc(v.values[2]) :
                                                                   r.fk));
            return VOID;
        }
    }
    
    public int getState() {
        return state;
    }
    
    public void start() {
        thread.start();
    }
    
    public void run() {
        threadContext=Context.lookupThreadContext();
        Interpreter interp = Context.enter(env);
        Context.lookupThreadContext().hostThread=new WeakReference(this);

        state=RUNNING;
        synchronized(this) {
            this.notify();
        }
        try {
            rv=interp.eval(thunk,new Value[]{});
            state=FINISHED;
        } catch (SchemeException se) {
            rv=new Values(new Value[] {se.m, se.e});
            state=FINISHED_ABNORMALLY;
        } 
        Context.exit();
    }
    
    public void display(ValueWriter w) throws IOException {
        displayNamedOpaque(w, "thread");
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
