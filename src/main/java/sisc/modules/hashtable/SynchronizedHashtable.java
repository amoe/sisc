package sisc.modules.hashtable;

import sisc.data.*;

import java.io.IOException;

import sisc.io.ValueWriter;
import sisc.ser.Serializer;
import sisc.ser.Deserializer;
import sisc.util.ExpressionVisitor;
import sisc.modules.Threads.Mutex;

public class SynchronizedHashtable extends HashtableBase {

    private HashtableBase delegate;

    public SynchronizedHashtable() {}

    public SynchronizedHashtable(HashtableBase delegate) {
        this.delegate = delegate;
    }

    public HashtableBase getDelegate() {
        return delegate;
    }

    public Procedure getEqualsProc() {
        return delegate.getEqualsProc();
    }

    public Procedure getHashProc() {
        return delegate.getHashProc();
    }

    public Value get(Value k) {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.get(k);
        } finally {
            m.unlock();
        }
    }

    public Value put(Value k, Value v) {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.put(k, v);
        } finally {
            m.unlock();
        }
    }

    public Value remove(Value k) {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.remove(k);
        } finally {
            m.unlock();
        }
    }

    public int size() {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.size();
        } finally {
            m.unlock();
        }
    }

    public void clear() {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            delegate.clear();
        } finally {
            m.unlock();
        }
    }

    public void addAList(Pair p) {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            delegate.addAList(p);
        } finally {
            m.unlock();
        }
    }

    public Pair toAList() {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.toAList();
        } finally {
            m.unlock();
        }
    }

    public Pair keys() {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.keys();
        } finally {
            m.unlock();
        }
    }

    public boolean valueEqual(Value v) {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.valueEqual(v);
        } finally {
            m.unlock();
        }
    }

    public int valueHashCode() {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.valueHashCode();
        } finally {
            m.unlock();
        }
    }

    public void serialize(Serializer s) throws IOException {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            s.writeExpression(delegate);
        } finally {
            m.unlock();
        }
    }

    public void deserialize(Deserializer s) throws IOException {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            delegate = (HashtableBase)s.readExpression();
        } finally {
            m.unlock();
        }
    }

    public boolean visit(ExpressionVisitor v) {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            return delegate.visit(v);
        } finally {
            m.unlock();
        }
    }

    public void display(ValueWriter w) throws IOException {
        Mutex m = Mutex.of(this);
        m.acquire();
        try {
            delegate.display(w);
        } finally {
            m.unlock();
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
