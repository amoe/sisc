package sisc.io;

import java.io.IOException;
import java.io.Writer;

import sisc.data.Value;
import sisc.util.Util;
import sisc.util.ExpressionVisitor;
import sisc.util.ExpressionVisitee;

public class PortValueWriter extends Util
    implements ValueWriter, ExpressionVisitor {

    private Writer port;

    private boolean display;

    private boolean vectorLengthPrefixing, caseSensitive;
    private String lineSeparator;

    public PortValueWriter(Writer port,
                           boolean vectorLengthPrefixing,
                           boolean caseSensitive) {
        this.port = port;
        this.vectorLengthPrefixing = vectorLengthPrefixing;
        this.caseSensitive = caseSensitive;
        this.lineSeparator = System.getProperty("line.separator");
    }

    protected void displayOrWrite(Value v, boolean display)
        throws IOException {

        this.display = display;
        append(v);
    }

    public void display(Value v) throws IOException {
        displayOrWrite(v, true);
    }

    public void write(Value v) throws IOException {
        displayOrWrite(v, false);
    }

    public boolean visit(ExpressionVisitee e) {
        return true;
    }

    public ValueWriter append(Value v) throws IOException {
        if (display)
            v.display(this);
        else
            v.write(this);
        return this;
    }

    public ValueWriter append(char c) throws IOException {
        char nl = this.lineSeparator.charAt(this.lineSeparator.length() - 1);
        port.write(c);
        if (c == nl)  port.flush();
        return this;
    }

    public ValueWriter append(String s) throws IOException {
        port.write(s);
        if (s.contains(this.lineSeparator)) port.flush();
        return this;
    }

    public boolean isInlinable(Value v) {
        return true;
    }

    public boolean vectorLengthPrefixing() {
        return vectorLengthPrefixing;
    }

    public boolean caseSensitive() {
        return caseSensitive;
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
