package sisc.interpreter;

import sisc.data.*;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Iterator;

import sisc.ser.Deserializer;
import sisc.ser.Serializer;
import sisc.util.ExpressionVisitee;
import sisc.util.ExpressionVisitor;
import sisc.util.Util;

public class StackTracer implements Cloneable, ExpressionVisitee {

    private static Symbol UNKNOWN = Symbol.get("?");

    private int maxDepth = 16;

    private List stack = new ArrayList();
    private List toAdd = new ArrayList();

    private boolean overflown = false;

    private static class Wrapper {

        public Expression expr;

        public Wrapper(Expression expr) {
            this.expr = expr;
        }

        public int hashCode() {
            return System.identityHashCode(expr);
        }

        public boolean equals(Object o) {
            return (o instanceof Wrapper) &&
                ((Wrapper)o).expr == expr;
        }

    }

    private static class Repetition implements Cloneable {
        public int count;
        public List exprs;

        public Repetition(int count, List exprs) {
            this.count = count;
            this.exprs = exprs;
        }

        public boolean equals(Object o) {
            if (!(o instanceof Repetition)) return false;
            Repetition rp = (Repetition)o;
            return rp.count == count && rp.exprs.equals(exprs);
        }

        public Object clone() throws CloneNotSupportedException {
            return new Repetition(count, exprs);
        }

        public Repetition copy() {
            try {
                return (Repetition)clone();
            } catch (CloneNotSupportedException e) {
                return this;
            }
        }

    }

    public StackTracer(int maxDepth) {
        this.maxDepth = maxDepth;
    }

    // No arg constructor for serialization/deserialization
    public StackTracer() {
	}

	private static boolean addTailToPreceedingRepetition(List l) {
        /*
          (... (n x_0 ... x_m) x_0 ... x_m)
          -->
          (... (n+1 x_0 ... x_m))
        */
        int sz = l.size();
        for (int i = sz-2; i >= 0; i--) {
            Object o = l.get(i);
            if (!(o instanceof Repetition)) continue;
            Repetition rp = (Repetition)o;
            List sl = l.subList(i+1, sz);
            if (rp.exprs.equals(sl)) {
                rp.count++;
                sl.clear();
                return true;
            }
        }

        return false;
    }

    private static boolean createRepetitionFromTail(List l) {
        /*
          (... x_0 ... x_m x_0 ... x_m)
          -->
          (... (2 x_0 ... x_m))
        */
        int sz = l.size();
        for (int i = sz-1; i >= (sz+1)/2; i--) {
            List sl = l.subList(i, sz);
            if (sl.equals(l.subList(2*i-sz, i))) {
                Repetition rp = new Repetition(2, new ArrayList(sl));
                l.subList(2*i-sz, sz).clear();
                l.add(rp);
                return true;
            }
        }

        return false;
    }

    private static void compact(List l) {
        /*
          This algorithm was kindly donated by Paul Crowley
          <paul@ciphergoth.org>
        */
        while(addTailToPreceedingRepetition(l) ||
              createRepetitionFromTail(l));
    }

    private void addAll() {
        for (Iterator i = toAdd.iterator(); i.hasNext(); ) {
            Expression e = (Expression)i.next();
            stack.add(new Wrapper(e));
            compact(stack);
        }
        toAdd.clear();
    }

    public void add(Expression expr) {
        toAdd.add(expr);
        //TODO: we should look at the *deep* size of stack here
        if (toAdd.size() + stack.size() > maxDepth) {
            addAll();
            if (stack.size() > maxDepth) {
                stack.subList(0, maxDepth/2).clear();
                overflown = true;
            }
        }
    }

    public void clear() {
        stack.clear();
        toAdd.clear();
        overflown = false;
    }

    public Object clone() throws CloneNotSupportedException {
        StackTracer res = new StackTracer(maxDepth);
        res.toAdd.addAll(toAdd);
        //it's sufficient to create copies of top-level Repetitions,
        //since these are the only objects that can get modified.
        for (Iterator i = stack.iterator(); i.hasNext(); ) {
            Object o = i.next();
            res.stack.add((o instanceof Repetition) ?
                          ((Repetition)o).copy() :
                          o);
        }
        res.overflown = overflown;
        return res;
    }

    public StackTracer copy() {
        try {
            return (StackTracer)clone();
        } catch (CloneNotSupportedException e) {
            return this;
        }
    }

    private static Value deepListToValue(List l) {
        Pair res = Util.EMPTYLIST;
        for (Iterator i = l.iterator(); i.hasNext(); ) {
            Object o = i.next();
            if (o instanceof Repetition) {
                Repetition rp = (Repetition)o;
                res = new Pair(new Pair(Quantity.valueOf(rp.count),
                                        deepListToValue(rp.exprs)),
                               res);
            } else {
                res = new Pair(new ExpressionValue(((Wrapper)o).expr),
                               res);
            }

        }

        return res;
    }

    public Value toValue() {
        addAll();
        return new Pair(SchemeBoolean.get(overflown), deepListToValue(stack));
    }

	public void serialize(Serializer s) throws IOException {
		s.writeInt(maxDepth);
		s.writeBoolean(overflown);
		writeList(s, toAdd);
		writeList(s, stack);
	}

	private static final int WRAPPER=0, REPETITION=1, EXPR=2;
	
	private static void writeList(Serializer s, List ls) throws IOException {
		s.writeInt(ls.size());
		for (int i=0; i<ls.size(); i++) {			
			Object o=ls.get(i);
			if (o instanceof Wrapper) {
				s.writeInt(WRAPPER);
				s.writeExpression(((Wrapper)o).expr);
			} else if (o instanceof Repetition) {
				s.writeInt(REPETITION);
				Repetition r=(Repetition)o;
				s.writeInt(r.count);
				writeList(s, r.exprs);
			} else if (o instanceof Expression) {
				s.writeInt(EXPR);
				s.writeExpression((Expression)o);
			}
		}
	}

	private static List readList(Deserializer s) throws IOException {
		int size=s.readInt();
		List rv=new ArrayList(size);
		for (int i=0; i<size; i++) {
			int type=s.readInt();
			switch (type) {
			case WRAPPER:
				Expression e=s.readExpression();
				rv.add(new Wrapper(e));
				break;
			case REPETITION:
				int count=s.readInt();
				List exprs=readList(s);
				rv.add(new Repetition(count, exprs));
				break;
			case EXPR:
				rv.add(s.readExpression());
			}
		}
		return rv;
	}

	public void deserialize(Deserializer s) throws IOException {
		maxDepth=s.readInt();
		overflown=s.readBoolean();
		toAdd=readList(s);
		stack=readList(s);
	}

	
	private static boolean visitList(ExpressionVisitor v, List ls) {
		boolean rv=true;
		for (int i=0; i<ls.size(); i++) {			
			Object o=ls.get(i);
			if (o instanceof Wrapper) {
				rv = v.visit(((Wrapper)o).expr) && rv;
			} else if (o instanceof Repetition) {
				Repetition r=(Repetition)o;
				rv = visitList(v, r.exprs) && rv;
			} else if (o instanceof Expression) {
				rv = v.visit((Expression)o) && rv;
			}
			if (!rv) return false;
		}
		return rv;
	}

	public boolean visit(ExpressionVisitor v) {
		return visitList(v, toAdd) && visitList(v, stack);
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
