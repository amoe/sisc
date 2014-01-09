package sisc.interpreter;

import java.lang.ref.*;
import java.util.*;

import sisc.data.SchemeThread;
import sisc.util.Util;
import sisc.env.DynamicEnvironment;

public class ThreadContext extends Util {

    public static class State {
        public final Interpreter interpreter;
        public final ClassLoader classLoader;

        public State(Interpreter r, ClassLoader cl) {
            this.interpreter = r;
            this.classLoader = cl;
        }
    }

    protected Stack states = new Stack();
    protected Random r = new Random();
    
    public WeakReference hostThread;
    public boolean interrupt = false;
    public long unicityMajor=genUnicityMajor();
    public char unicityMinor=0;

    public ThreadContext() {}

    /*********** Unique Value Support ********************/
    protected long genUnicityMajor() {
        return System.currentTimeMillis() + 
            ((r.nextInt() & 0xffff)*311040000000L);
    }

    public long nextUnique() {
        if (++unicityMinor == 0) 
            unicityMajor=genUnicityMajor();
        return unicityMajor + (unicityMinor*31104000000L);
    }

    /*********** state stack maintenance ***********/

    public Interpreter currentInterpreter() {
        return (states.empty() ?
                null : ((State)states.peek()).interpreter);
    }

    public Interpreter currentInterpreter(AppContext ctx) {
        for (Iterator it = states.iterator(); it.hasNext();) {
            Interpreter r = ((State)it.next()).interpreter;
            if (r.dynenv.ctx == ctx) return r;
        }
        return null;
    }

    protected void pushState(State s) {
        states.push(s);
    }

    protected State popState() {
        return (State)states.pop();
    }

    public Thread nativeThread() {
        if (hostThread==null) return null;
        SchemeThread st=(SchemeThread)hostThread.get();
        if (st==null) return null;
        else return st.thread;
    }
    
    public void setHostThread(DynamicEnvironment dynenv, Thread thread) {
        if (nativeThread() != thread) {
            //Wrap the thread in a SchemeThread with no thunk.
            SchemeThread st=new SchemeThread(dynenv, null);
            st.thread=thread;
            hostThread = new WeakReference(st);
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
