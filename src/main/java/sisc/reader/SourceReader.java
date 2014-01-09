/*
 * $Id: SourceReader.java,v 1.3 2007/02/06 18:45:37 scgmille Exp $
 */
package sisc.reader;

import java.io.IOException;
import java.io.PushbackReader;
import java.io.Reader;

public class SourceReader extends PushbackReader {

    public SourceReader(Reader in, String file) {
        super(in);
        line=1;
        column=1;
        sourceFile=file;
    }

    public int line, column;
    public String sourceFile;
   
    protected void maintainLineColumn(int c) {
        if (c=='\n') {
            line++;
            column=1;
        } else if (c=='\t') 
            column+=8;
        else column++;
    }
    
    //Does this even work?
    protected void unmaintainLineColumn(int c) {
        if (c=='\n') {
            line--;
            column=-1;
        } else if (c=='\t') 
            column-=8;
        else column--;
    }
    
    public int read() throws IOException {
        int c=super.read();
        maintainLineColumn(c);
        return c;
    }
 
    public int read(char[] buffer) throws IOException {
        return read(buffer, 0, buffer.length);
    }
    
    //TODO: Could be made more efficient by not doing character by character scan
    public int read(char[] buffer, int offset, int length) throws IOException {
        int count=super.read(buffer, offset, length);
        for (int i=offset; i<count; i++) {
            maintainLineColumn(buffer[i]);
        }
        return count;
    }
    
    public void unread(int c) throws IOException {
        super.unread(c);
        unmaintainLineColumn(c);
    }

    public void unread(char[] buffer) throws IOException {
        unread(buffer, 0, buffer.length);
    }
    
    public void unread(char[] buffer, int offset, int length) throws IOException {
        super.unread(buffer, offset, length);
        for (int i=offset; i<length; i++) {
            unmaintainLineColumn(buffer[i]);
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
