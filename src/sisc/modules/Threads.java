package sisc.modules;

import java.io.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.nativefun.*;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.util.ExpressionVisitor;
import sisc.util.Util;

public class Threads extends Util {

    protected static Symbol S_READY = Symbol.get("ready"),
        S_RUNNING = Symbol.get("running"),
        S_FINISHED = Symbol.get("finished"),
        S_FINISHED_ABNORMALLY = Symbol.get("finished-with-error");


    protected static final Symbol THREADB =
        Symbol.intern("sisc.modules.Messages");

    protected static final Symbol MUTEX = Symbol.get("mutex");

    public static final SchemeThread sthread(Value o) {
        try {
            return (SchemeThread)o;
        } catch (ClassCastException e) { typeError(THREADB, 
                                                   "sthread", o); }
        return null;
    }

    public static final Mutex mutex(Value o) {
        try {
            return (Mutex)o;
        } catch (ClassCastException e) { typeError(THREADB,
                                                   "mutex", o); }
        return null;
    }

    public static final CondVar condvar(Value o) {
        try {
            return (CondVar)o;
        } catch (ClassCastException e) { typeError(THREADB, 
                                                   "condvar", o); }
        return null;
    }

    static Symbol stateOf(SchemeThread c) {
        switch (c.getState()) {
        case SchemeThread.READY:
            return S_READY;
        case SchemeThread.RUNNING:
            return S_RUNNING;
        case SchemeThread.FINISHED:
            return S_FINISHED;
        case SchemeThread.FINISHED_ABNORMALLY:
            return S_FINISHED_ABNORMALLY;
        default:
            return null;
        }
    } 

    public static class CondVar extends Value implements NamedValue {

        public CondVar() {}

        public void display(ValueWriter w) throws IOException {
            displayNamedOpaque(w, "condvar");
        }

        public void c_notify() {
            synchronized(this) {
                notify();
            }
        }

        public void c_notifyall() {
            synchronized(this) {
                notifyAll();
            }
        }
    }

    public static class Mutex extends Value implements NamedValue {
        private transient int lockCount=0;
        private transient Thread owner=null;

        public Value lock(long timeout) {
            Thread thisThread=null;

            synchronized(this) {
                if (owner == null) {
                    owner=Thread.currentThread();
                    
                } else if ((thisThread=Thread.currentThread())!= 
                           owner) {
                    
                    long lastAwoken=System.currentTimeMillis();
                    
                    while (lockCount>0) {
                        try {
                            this.wait(timeout);
                        } catch (InterruptedException ie) {}
                        
                        long now=System.currentTimeMillis();
                        timeout-=(now-lastAwoken);
                        if (timeout <= 0)
                            return FALSE;
                        else
                            lastAwoken=now;
                    }
                    owner=thisThread;
                }
                lockCount++;
                return TRUE;
            }
        }

        public final Value acquire() {
            Thread thisThread=Thread.currentThread();

            if (owner==thisThread) 
                lockCount++;
            else {
                synchronized(this) {
                    while (lockCount>0) {
                        try {
                            this.wait();
                        } catch (InterruptedException ie) {}
                        
                    }
                    lockCount++;
                }
                owner=thisThread;
            }

            
            return TRUE;
        }

        public final void unlock() {
            if (owner==Thread.currentThread()) {
                synchronized(this) {
                    if ((--lockCount)==0) {
                        owner=null;
                        this.notify();
                    }
                }
            }
        }

        public final void unlockAll() {
            if (owner==Thread.currentThread()) {
                synchronized(this) {
                    lockCount=0;
                    owner=null;
                    this.notify();
                }
            }
        }

        public void unlock(CondVar condvar) {
            while (true) {
                try {
                    synchronized(condvar) {
                        unlockAll();
                        condvar.wait();
                    }
                    return;
                } catch (InterruptedException e) {}
            }
        }

        public Value unlock(CondVar condvar, long timeout) {
            while (true) {
                try {
                    synchronized(condvar) {
                        unlockAll();
                        long start=System.currentTimeMillis();
                        condvar.wait(timeout);
                        // This is only a best guess.
                        if ((System.currentTimeMillis() - start) >= timeout)
                            return FALSE;
                    }
                    return TRUE;
                } catch (InterruptedException e) {}
            }
        }
        public void display(ValueWriter w) throws IOException {
            displayNamedOpaque(w, "mutex");
        }

        public static Mutex of(Value v) {
            synchronized(v) {
                Value m = v.getAnnotation(MUTEX);
                if (m==FALSE) {
                    m=new Mutex();
                    v.setAnnotation(MUTEX, m);
                }
                return mutex(m);
            }
        }

        public Mutex() {}

        public void serialize(Serializer ser) throws IOException {
            if (lockCount > 0 || owner != null)
               System.err.println(warn("serializinglockedmutex"));
        }

        public boolean visit(ExpressionVisitor v) {
            return super.visit(v);
        }
    }

    public static class Index extends IndexedLibraryAdapter {

        public Value construct(Object context, int id) {
            if (context == null || context == Simple.class) {
                return new Simple(id);            
            } else return new Complex(id);
        }

        public Index() {
            define("thread/daemon!", Complex.class, SETTHREADDAEMON);
            define("thread/interrupt", Complex.class, THREADINTERRUPT);
            define("thread/join", Complex.class, THREADJOIN);
            define("thread/name!", Complex.class, SETTHREADNAME);
            define("thread/new", Complex.class, THREADNEW);
            define("thread/notify", Complex.class, THREADNOTIFY);
            define("thread/notify-all", Complex.class, THREADNOTIFYALL);
            define("thread/priority!", Complex.class, SETTHREADPRIORITY);
            define("thread/result", Complex.class, THREADRESULT);
            define("thread/_set-result!", Complex.class, THREADSETRESULT);
            define("thread/start", Complex.class, THREADSTART);
            define("thread/yield", Complex.class, THREADYIELD);

            define("condvar/notify", Complex.class, CONDVARNOTIFY);
            define("condvar/notify-all", Complex.class, CONDVARNOTIFYALL);

            define("mutex/lock!", Complex.class, MUTEXLOCK);
            define("mutex/unlock!", Complex.class, MUTEXUNLOCK);

            define("thread?", THREADQ);
            define("thread/current", THREADCURRENT);
            define("thread/wait", THREADWAIT);
            define("thread/name", THREADNAME);
            define("thread/daemon?", THREADDAEMONQ);
            define("thread/priority",THREADPRIORITY);
            define("thread/state", THREADSTATE);
            define("thread/interrupted?", THREADINTERRUPTEDQ);
            define("thread/holds-lock?", THREADHOLDSLOCKQ);

            define("thread/_active-thread-count", THREADSRUNNING);

            define("condvar?", CONDVARQ);
            define("condvar/new", CONDVARNEW);
            
            define("mutex?", MUTEXQ);
            define("mutex-of", MUTEXOF);
            define("mutex/new", MUTEXNEW);
        }
    }

    /**
     * The Simple procedures are purely functional procedures
     * which do not need to access interpreter registers to execute
     */
    public static class Simple extends IndexedFixableProcedure {
        public Simple() {}

        Simple(int id) {
            super(id);
        }
        public Value apply() throws ContinuationException {
            switch(id) {
            case MUTEXNEW:
                return new Mutex();
            case CONDVARNEW:
                return new CondVar();
            case THREADCURRENT:
                SchemeThread t=(SchemeThread)Context.lookupThreadContext().hostThread.get();
                if (t==null) return FALSE;
                else return t;
            default:
                throwArgSizeException();
            }
            return VOID;
        }
        
        public Value apply(Value v1) throws ContinuationException {
            switch(id) {
            case THREADQ:
                return truth(v1 instanceof SchemeThread);
            case MUTEXQ:
                return truth(v1 instanceof Mutex);
            case CONDVARQ:
                return truth(v1 instanceof CondVar);
            case MUTEXOF:
                return Mutex.of(v1);
            case THREADINTERRUPTEDQ:
                return truth(sthread(v1).threadContext.interrupt);
            case THREADNAME:
                return new SchemeString(sthread(v1).thread.getName());
            case THREADPRIORITY:
                return Quantity.valueOf(sthread(v1).thread.getPriority());
            case THREADDAEMONQ:
                return truth(sthread(v1).thread.isDaemon());
            case THREADSTATE:
                SchemeThread c=sthread(v1);
                return stateOf(c);
            default:
                throwArgSizeException();
            }
            return VOID;
        }
        
        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch(id) {
            case THREADHOLDSLOCKQ:
                return truth(mutex(v2).owner==sthread(v1).thread);
            default:
                throwArgSizeException();
            }
            return VOID;
        }
    }
    
    /**
     * The Complex procedures either have a side effect, or
     * require the interpreter to execute
     */
    public static class Complex extends CommonIndexedProcedure {
        public Complex() {}
      
        Complex(int id) {
            super(id);
        }

        public Value apply() throws ContinuationException {
            switch(id) {
            case THREADYIELD:
                Thread.yield();
                return VOID;
            default:
                throwArgSizeException();                    
            }
            return VOID;
        }

        public Value apply(Interpreter f, Value v1) throws ContinuationException {
            switch(id) {
            case THREADNEW:
                return new SchemeThread(f.dynenv, (Procedure) f.vlr[0]);
            case THREADRESULT:
                return sthread(f.vlr[0]).getResult(f);
            case THREADINTERRUPT:
                SchemeThread c=sthread(v1);
                c.thread.interrupt();
                c.threadContext.interrupt=true;
                return VOID;
            case CONDVARNOTIFY:
                condvar(v1).c_notify();
                return VOID;
            case CONDVARNOTIFYALL:
                condvar(v1).c_notifyall();
                return VOID;
            case MUTEXLOCK:
                return mutex(v1).acquire();
            case MUTEXUNLOCK:
                mutex(v1).unlock();
                return TRUE;
            case THREADSTART:
                c=sthread(v1);
                c.start();
                while (c.state==SchemeThread.READY) {
                    synchronized(c) {
                        try {
                            c.wait(500);
                        } catch (InterruptedException e) {}
                    }
                }
                return VOID;
            case THREADJOIN:
                c=sthread(v1);
                if (c.state>=SchemeThread.RUNNING) {
                    try {
                        c.thread.join();
                    } catch (InterruptedException ie) {}
                    if (c.state==SchemeThread.RUNNING) 
                        return FALSE;
                    else return stateOf(c);
                } else {
                    throwPrimException(liMessage(THREADB,"threadnotstarted"));
                }
            default:
                throwArgSizeException();                    
            }
            return VOID;
        }
        
        public Value apply(Value v1, Value v2) throws ContinuationException {
            switch(id) {
            case MUTEXUNLOCK:
                mutex(v1).unlock(condvar(v2));
                return TRUE;
            case MUTEXLOCK:
                long timeout=((Quantity) v2).longValue();
                return mutex(v1).lock(timeout);
            case THREADJOIN:
                SchemeThread c=sthread(v1);

                if (c.state>=SchemeThread.RUNNING) {
                    try {
                        c.thread.join(((Quantity) v2).indexValue());
                    } catch (InterruptedException ie) {}
                    if (c.state==SchemeThread.RUNNING) 
                        return FALSE;
                    else return stateOf(c);
                } else {
                    throw new RuntimeException(liMessage(THREADB,"threadnotstarted"));
                }
            case THREADSETRESULT:
                sthread(v1).rv=v2;
                return VOID;
            case SETTHREADPRIORITY:
                sthread(v1).thread.setPriority(((Quantity) v2).indexValue());
                return VOID;
            case SETTHREADDAEMON:
                sthread(v1).thread.setDaemon(truth(v2));
                return VOID;
            case SETTHREADNAME:
                c=sthread(v1);
                c.thread.setName(SchemeString.asString(v2));
                return VOID;
            default:
                throwArgSizeException();                    
            }
            return VOID;
        }
        
        public Value apply(Value v1, Value v2, Value v3) throws ContinuationException {
            switch(id) {
            case MUTEXUNLOCK:
                return mutex(v1).unlock(condvar(v2), ((Quantity) v3).longValue());
            default:
                throwArgSizeException();                    
            }
            return VOID;
        }                
    }
    
    protected static final int // Next: 33 
        THREADNEW = 0,
        THREADRESULT = 21,
        THREADSTART = 1,
        THREADYIELD = 2,
        THREADSLEEP = 3,
        THREADINTERRUPT = 4,
        THREADJOIN = 5,
        THREADCURRENT = 6,
        THREADQ = 7,
        THREADNOTIFY = 8,
        THREADNOTIFYALL = 9,
        THREADWAIT = 10,
        THREADNAME = 11,
        THREADPRIORITY = 12,
        THREADDAEMONQ = 13,
        SETTHREADNAME = 14,
        SETTHREADPRIORITY = 15,
        SETTHREADDAEMON = 16,
        THREADSTATE = 17,
        THREADINTERRUPTEDQ = 18,
        THREADHOLDSLOCKQ = 19,
        THREADSRUNNING = 20,
        MUTEXNEW = 22,
        MUTEXLOCK = 23,
        MUTEXUNLOCK = 24,
        CONDVARNOTIFY = 25,
        CONDVARNOTIFYALL = 26,
        MUTEXOF = 28,
        MUTEXQ = 29,
        CONDVARQ = 30,
        CONDVARNEW = 31,
        THREADSETRESULT = 32;

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
