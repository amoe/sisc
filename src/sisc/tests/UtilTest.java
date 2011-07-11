package sisc.tests;

import sisc.data.Expression;
import sisc.data.Pair;
import sisc.data.Symbol;
import sisc.data.Value;
import sisc.util.Util;
import junit.framework.TestCase;

/**
 * @author Marvin H. Sielenkemper
 * @version $Revision: 1.6 $
 *
 * This class contains tests for the commonly used methods from
 * sisc.util.Util.
 * 
 */
public class UtilTest extends TestCase
{
    private Value va;
    private Value vb;
    private Value vc;
    
    private Pair l0;
    private Pair l1;
    private Pair l2;
    private Pair l3;

    protected void setUp() throws Exception
    {
        super.setUp();
        
        va = Util.EOF;
        vb = Util.read("1");
        vc = Util.javaWrap(new Object());
        
        l0 = Util.EMPTYLIST;
        l1 = Util.list(va);
        l2 = Util.list(va, vb);
        l3 = Util.list(va, vb, vc);
    }
    
    protected void tearDown() throws Exception
    {
        l3 = l2 = l1 = l0 = null;
        va = vb = vc = null;
        super.tearDown();
    }

    
    public void testSafeGetProperty()
    {
    //TODO Implement safeGetProperty().
    }

    /*
     * Class to test for String warn(String)
     */
    public void testWarnString()
    {
    //TODO Implement warn().
    }

    /*
     * Class to test for String warn(String, String)
     */
    public void testWarnStringString()
    {
    //TODO Implement warn().
    }

    public void testJavaWrap()
    {
    //TODO Implement javaWrap().
    }

    /*
     * Class to test for void error(Interpreter, Value, String, Pair)
     */
    public void testErrorInterpreterValueStringPair()
    {
    //TODO Implement error().
    }

    /*
     * Class to test for void error(Interpreter, Value, String, Exception)
     */
    public void testErrorInterpreterValueStringException()
    {
    //TODO Implement error().
    }

    /*
     * Class to test for void error(Interpreter, Value, String)
     */
    public void testErrorInterpreterValueString()
    {
    //TODO Implement error().
    }

    /*
     * Class to test for void error(Interpreter, String, Pair)
     */
    public void testErrorInterpreterStringPair()
    {
    //TODO Implement error().
    }

    /*
     * Class to test for void error(Interpreter, String)
     */
    public void testErrorInterpreterString()
    {
    //TODO Implement error().
    }

    /*
     * Class to test for void error(Interpreter, Value)
     */
    public void testErrorInterpreterValue()
    {
    //TODO Implement error().
    }

    public void testCurrentClassLoader()
    {
    //TODO Implement currentClassLoader().
    }

    public void testRead()
    {
    //TODO Implement read().
    }

    /*
     * Class to test for void error(Interpreter, Pair)
     */
    public void testErrorInterpreterPair()
    {
    //TODO Implement error().
    }

    public void testJustify()
    {
    //TODO Implement justify().
    }

    public void testArgCheck()
    {
    //TODO Implement argCheck().
    }

    public void testUpdateName()
    {
    //TODO Implement updateName().
    }

    public void testLength()
    {
        assertEquals(0, Util.length(l0));
        assertEquals(1, Util.length(l1));
        assertEquals(2, Util.length(l2));
        assertEquals(3, Util.length(l3));
    }

    public void testPairToExpressions()
    {
        Expression[] es = Util.pairToExpressions(l3);
        assertEquals(es.length, 3);
        assertSame(es.getClass().getComponentType(), Expression.class);
        assertSame(es[0], va);
        assertSame(es[1], vb);
        assertSame(es[2], vc);
    }

    public void testPairToValues()
    {
        Value[] vs = Util.pairToValues(l3);
        assertEquals(3, vs.length);
        assertSame(vs.getClass().getComponentType(), Value.class);
        assertSame(vs[0], va);
        assertSame(vs[1], vb);
        assertSame(vs[2], vc);
    }

    public void testArgsToSymbols()
    {
        assertEquals(Util.argsToSymbols(l0).length, 0);

        Symbol sa = Symbol.get("Sa");
        Symbol sb = Symbol.get("Sb");
        Symbol sc = Symbol.get("Sc");

        Pair pa = Util.list(sa, sb, sc);
        
        Symbol[] ssa = Util.argsToSymbols(pa);
        assertEquals(ssa.length, 3);
        assertSame(ssa.getClass().getComponentType(), Symbol.class);
        assertSame(ssa[0], sa);
        assertSame(ssa[1], sb);
        assertSame(ssa[2], sc);

        Pair pb = new Pair(sa, new Pair(sb, sc));

        Symbol[] ssb = Util.argsToSymbols(pb);
        assertEquals(ssb.length, 3);
        assertSame(ssb.getClass().getComponentType(), Symbol.class);
        assertSame(ssb[0], sa);
        assertSame(ssb[1], sb);
        assertSame(ssb[2], sc);

        try
        {
            Util.argsToSymbols(l1);
            fail("ClassCastException expected");
        }
        catch (ClassCastException e)
        { }
    }

    /*
     * Class to test for void typeError(String, Value)
     */
    public void testTypeErrorStringValue()
    {
    //TODO Implement typeError().
    }

    /*
     * Class to test for void typeError(Symbol, String, Value)
     */
    public void testTypeErrorSymbolStringValue()
    {
    //TODO Implement typeError().
    }

    public void testSym()
    {
    //TODO Implement sym().
    }

    public void testSymval()
    {
    //TODO Implement symval().
    }

    public void testNum()
    {
    //TODO Implement num().
    }

    public void testPair()
    {
        assertSame(Util.pair(l0), l0);
        assertSame(Util.pair(l1), l1);
        assertSame(Util.pair(l2), l2);
        assertSame(Util.pair(l3), l3);
        
        try
        {
            Util.pair(va);
            fail("expected a RuntimeException");
        }
        catch (RuntimeException e)
        { }

        try
        {
            Util.pair(vb);
            fail("expected a RuntimeException");
        }
        catch (RuntimeException e)
        { }
    }

    public void testProc()
    {
    //TODO Implement proc().
    }

    public void testTruePair()
    {
        try
        {
            Util.truePair(l0);
            fail("expected a RuntimeException");
        }
        catch (RuntimeException e)
        { }

        assertSame(Util.truePair(l1), l1);
        assertSame(Util.truePair(l2), l2);
        assertSame(Util.truePair(l3), l3);
    }

    public void testCharacter()
    {
    //TODO Implement character().
    }

    public void testChr()
    {
    //TODO Implement chr().
    }

    public void testString()
    {
    //TODO Implement string().
    }

    public void testStr()
    {
    //TODO Implement str().
    }

    public void testSymbol()
    {
    //TODO Implement symbol().
    }

    public void testVec()
    {
    //TODO Implement vec().
    }

    public void testOutport()
    {
    //TODO Implement outport().
    }

    public void testInport()
    {
    //TODO Implement inport().
    }

    public void testEnv()
    {
    //TODO Implement env().
    }

    public void testBox()
    {
    //TODO Implement box().
    }

    public void testCont()
    {
    //TODO Implement cont().
    }

    public void testAnnotated()
    {
    //TODO Implement annotated().
    }

    /*
     * Class to test for URL url(Value)
     */
    public void testUrlValue()
    {
    //TODO Implement url().
    }

    /*
     * Class to test for URL url(String)
     */
    public void testUrlString()
    {
    //TODO Implement url().
    }

    /*
     * Class to test for URL url(Value, Value)
     */
    public void testUrlValueValue()
    {
    //TODO Implement url().
    }

    public void testNlib()
    {
    //TODO Implement nlib().
    }

    /*
     * Class to test for SchemeBoolean truth(boolean)
     */
    public void testTruthboolean()
    {
        assertSame(Util.truth(true), Util.TRUE);
        assertSame(Util.truth(false), Util.FALSE);
    }

    /*
     * Class to test for boolean truth(Value)
     */
    public void testTruthValue()
    {
        assertFalse(Util.truth(Util.FALSE));
        assertTrue(Util.truth(Util.TRUE));
        assertTrue(Util.truth(l0));
        assertTrue(Util.truth(l1));
        assertTrue(Util.truth(l2));
        assertTrue(Util.truth(l3));
        assertTrue(Util.truth(va));
        assertTrue(Util.truth(vb));
        assertTrue(Util.truth(vc));
    }

    public void testAssq()
    {
    //TODO Implement assq().
    }

    public void testMapcar()
    {
    //TODO Implement mapcar().
    }

    public void testReverse()
    {
    //TODO Implement reverse().
    }

    public void testReverseInPlace()
    {
    //TODO Implement reverseInPlace().
    }

    public void testAppend1()
    {
        assertSame(Util.append(l0, l0), l0);
        assertSame(Util.append(l0, l1), l1);
        assertSame(Util.append(l0, l2), l2);
        assertSame(Util.append(l0, l3), l3);
    }
    
    public void testAppend2()
    {
        Pair testee1 = Util.append(l1, l2);
        Pair testee2 = Util.append(l1, l2);

        assertNotSame(testee1, testee2);
        assertTrue(testee1.valueEqual(testee2));
        assertSame(testee1.cdr(), l2);
        assertSame(testee1.cdr(), testee2.cdr());
    }

    /*
     * Class to test for Pair list(Value)
     */
    public void testListValue()
    {
    //TODO Implement list().
    }

    /*
     * Class to test for Pair list(Value, Value)
     */
    public void testListValueValue()
    {
    //TODO Implement list().
    }

    /*
     * Class to test for Pair list(Value, Value, Value)
     */
    public void testListValueValueValue()
    {
    //TODO Implement list().
    }

    public void testValArrayToList()
    {
    //TODO Implement valArrayToList().
    }

    public void testMemq()
    {
    //TODO Implement memq().
    }

    public void testCollectionToList()
    {
    //TODO Implement collectionToList().
    }

    public void testRegisterBundle()
    {
    //TODO Implement registerBundle().
    }

    /*
     * Class to test for String liMessage(Symbol, String)
     */
    public void testLiMessageSymbolString()
    {
    //TODO Implement liMessage().
    }

    /*
     * Class to test for String liMessage(Symbol, String, String)
     */
    public void testLiMessageSymbolStringString()
    {
    //TODO Implement liMessage().
    }

    /*
     * Class to test for String liMessage(Symbol, String, String, String)
     */
    public void testLiMessageSymbolStringStringString()
    {
    //TODO Implement liMessage().
    }

    /*
     * Class to test for String liMessage(Symbol, String, String, String, String)
     */
    public void testLiMessageSymbolStringStringStringString()
    {
    //TODO Implement liMessage().
    }

    /*
     * Class to test for String liMessage(Symbol, String, String, String, String, String)
     */
    public void testLiMessageSymbolStringStringStringStringString()
    {
    //TODO Implement liMessage().
    }

    /*
     * Class to test for String liMessage(Symbol, String, String, int, int)
     */
    public void testLiMessageSymbolStringStringintint()
    {
    //TODO Implement liMessage().
    }

    /*
     * Class to test for String liMessage(Symbol, String, Object[])
     */
    public void testLiMessageSymbolStringObjectArray()
    {
    //TODO Implement liMessage().
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
 * Marvin H. Sielenkemper 
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
