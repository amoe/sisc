package sisc.modules.io;

import java.io.*;
import java.net.*;

import sisc.interpreter.*;
import sisc.nativefun.*;
import sisc.data.*;
import sisc.io.*;
import sisc.reader.SourceReader;
import sisc.util.Util;
import sisc.exprs.AnnotatedExpr;

public class IO extends IndexedProcedure {

    public static Symbol IOB =
        Symbol.intern("sisc.modules.io.Messages");

    protected static final int
        //NEXT = 37,

        ABSPATHQ            = 0,
        CHARREADY           = 3,
        CLOSEINPUTPORT      = 4,
        CLOSEOUTPUTPORT     = 5,
        DISPLAY             = 8,
        FILEEXISTSQ         = 9,
        FINDRESOURCE        = 6,
        FINDRESOURCES       = 2,
        FLUSHOUTPUTPORT     = 10,
        INPORTQ             = 12,
        INPORTLOCATION      = 13,
        LOAD                = 14,
        LOADEXPANDED        = 24,
        MAKEPATH            = 15,
        NORMALIZEURL        = 16,
        OPENBUFFEREDCHARINPORT = 32,
        OPENBUFFEREDCHAROUTPORT = 33,
        OPENCHARINPUTPORT   = 31,
        OPENCHAROUTPUTPORT   = 34,
        OPENINPUTFILE       = 17,
        OPENOUTPUTFILE      = 19,
        OPENSOURCEINPUTFILE = 20,
        OUTPORTQ            = 22,
        PEEKBYTE            = 30,
        PEEKCHAR            = 23,
        PORTQ               = 27,
        READ                = 21,
        READBYTE            = 29,
        READCHAR            = 18,
        READSTRING          = 25,
        READCODE            = 11,
        WRITE               = 1,
        WRITEBYTE           = 28,
        WRITECHAR           = 7,
        WRITESTRING         = 26;
        

    public static class Index extends IndexedLibraryAdapter { 
        
        public Value construct(Object context, int id) {
            return new IO(id);
        }
        
     public Index() {
            define("absolute-path?"     , ABSPATHQ);
            define("char-ready?"        , CHARREADY);
            define("close-input-port"   , CLOSEINPUTPORT);
            define("close-output-port"  , CLOSEOUTPUTPORT);
            define("display"            , DISPLAY);
            define("file-exists?"       , FILEEXISTSQ);
            define("find-resource"      , FINDRESOURCE);
            define("find-resources"     , FINDRESOURCES);
            define("flush-output-port"  , FLUSHOUTPUTPORT);
            define("input-port?"        , INPORTQ);
            define("input-port-location", INPORTLOCATION);
            define("load"               , LOAD);
            define("load-expanded"               , LOADEXPANDED);
            define("normalize-url"      , NORMALIZEURL);
            define("open-buffered-character-input-port", OPENBUFFEREDCHARINPORT);
            define("open-buffered-character-output-port", OPENBUFFEREDCHAROUTPORT);
            define("open-character-input-port", OPENCHARINPUTPORT);
            define("open-character-output-port", OPENCHAROUTPUTPORT);
            define("open-input-file"    , OPENINPUTFILE);
            define("open-output-file"   , OPENOUTPUTFILE);
            define("open-source-input-file", OPENSOURCEINPUTFILE);
            define("output-port?"       , OUTPORTQ);
            define("peek-byte"          , PEEKBYTE);
            define("peek-char"          , PEEKCHAR);
            define("port?"              , PORTQ);
            define("read"               , READ);
            define("read-byte"          , READBYTE);
            define("read-char"          , READCHAR);
            define("read-code"          , READCODE);
            define("read-string"        , READSTRING);
            define("write"              , WRITE);
            define("write-byte"         , WRITEBYTE);
            define("write-char"         , WRITECHAR);
            define("write-string"       , WRITESTRING);
        }
    }
    
    public IO(int id) {
        super(id);
    }
    
    public IO() {}
    
    public static void throwIOException(Interpreter f, String message, IOException e) 
        throws ContinuationException {
        if (f.acc == null) {
            error(f, message, list(new Pair(JEXCEPTION, javaWrap(e))));
        } else {
            if (f.acc.getName() != null) {
               error(f, f.acc.getName(), message, list(new Pair(JEXCEPTION, javaWrap(e))));
            } else {
               error(f, message, list(new Pair(JEXCEPTION, javaWrap(e))));
            }
        }
    }

    private static void maybeThrowErrorWithExprLocation(SchemeException se, Value v) {
        if (!(v instanceof AnnotatedExpr)) return;
        AnnotatedExpr aexp = (AnnotatedExpr)v;
        if (!(aexp.annotation instanceof Pair)) return;
        Pair anns = (Pair)aexp.annotation;
        Value sourceFile = assq(SOURCE_FILE, anns);
        Value sourceLine = assq(SOURCE_LINE, anns);
        Value sourceColumn = assq(SOURCE_COLUMN, anns);
        if (sourceFile == FALSE ||
            sourceLine == FALSE ||
            sourceColumn == FALSE)
            return;
        throwNestedPrimException(liMessage(IOB, "evalat",
                                           SchemeString.asString(((Pair) sourceFile).cdr()),
                                           ((Quantity) ((Pair) sourceLine).cdr()).intValue(),
                                           ((Quantity) ((Pair) sourceColumn).cdr()).intValue()),
                                 se);
    }

    private static Value readChar(Interpreter f, SchemeCharacterInputPort i) 
        throws ContinuationException {
        try {
            int c=i.getReader().read();
            if (c==-1) return EOF;
            return new SchemeCharacter((char)c);
        } catch (IOException e2) {
            throwIOException(f, liMessage(IOB, "errorreading", i.toString(),
                                          e2.getMessage()), e2);
        }
        return null; //Should never happen
    }

    private static Value peekChar(Interpreter f, SchemeCharacterInputPort i) 
    	throws ContinuationException {
    	try {
    		PushbackReader pbr=(PushbackReader)i.getReader();

    		Value v=readChar(f, i);
    		if (v instanceof SchemeCharacter) {
    			try {                    
    				pbr.unread(((SchemeCharacter)v).c);
    			} catch (IOException e) {
    				throwIOException(f, liMessage(IOB, "errorreading", 
    						i.toString()), e);
    			}	
    		}

            return v;
    	} catch (ClassCastException cce) {
        	throwPrimException(liMessage(IOB, "peeknotsupported",
                     i.toString()));
        	return VOID;
    	}
    }
    
    private static Value readByte(Interpreter f, SchemeBinaryInputPort i) throws ContinuationException {
        try {
            int c=i.getInputStream().read();
            if (c==-1) return EOF;
            return Quantity.valueOf(c);
        } catch (IOException e2) {
            throwIOException(f, liMessage(IOB, "errorreading", i.toString(),
                                          e2.getMessage()), e2);
        }
        return null; //Should never happen
    }
    
    private static Value peekByte(Interpreter f, SchemeBinaryInputPort i) throws ContinuationException {
    	try {
    		PushbackInputStream pbi=(PushbackInputStream)i.getInputStream();
            Value v=readByte(f, i);
            if (v instanceof Quantity) {
                try {
                    pbi.unread(((Quantity)v).indexValue());
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorreading", 
                                                  i.toString()), e);
                }
            }
            return v;
        } catch (ClassCastException cce) {
        	throwPrimException(liMessage(IOB, "peeknotsupported",
        			                     i.toString()));
        	return VOID;
        }
    }

    private static Value read(Interpreter r, SchemeCharacterInputPort i, int flags) 
        throws ContinuationException {
        try {
            return r.dynenv.parser.nextExpression((PushbackReader)i.getReader(), flags, r.dynenv.sourceAnnotations);
        } catch (EOFException e) {
            return EOF;
        } catch (IOException e2) {
            throwIOException(r, liMessage(IOB, "errorreading", i.toString(),
                                          e2.getMessage()), e2);
        } catch (ClassCastException cce) {
        	throwPrimException(liMessage(IOB, "peeknotsupported",
                    i.toString()));        	
        }
        return null; //Should never happen

    }
    
    public static Value read(Interpreter r, SchemeCharacterInputPort i) 
        throws ContinuationException {
        return read(r, i,
                    (r.dynenv.caseSensitive ? 
                    sisc.reader.Parser.CASE_SENSITIVE : 0) |
                    (r.dynenv.permissiveParsing ? 
                     sisc.reader.Parser.PERMISSIVE_PARSING : 0));
    }

    public static Value readCode(Interpreter r, SchemeCharacterInputPort i) 
        throws ContinuationException {
        return read(r, i,
                    sisc.reader.Parser.PRODUCE_ANNOTATIONS |
                    sisc.reader.Parser.PRODUCE_IMMUTABLES |
                    (r.dynenv.caseSensitive ? 
                     sisc.reader.Parser.CASE_SENSITIVE : 0) |
                    (r.dynenv.permissiveParsing ? 
                     sisc.reader.Parser.PERMISSIVE_PARSING : 0));
    }

    public Value displayOrWrite(Interpreter r,
                                SchemeCharacterOutputPort port,
                                Value v,
                                boolean display) 
        throws ContinuationException {
        try {
            ValueWriter w = r.dynenv.printShared ?
                new SharedValueWriter(port.getWriter(), r.dynenv.vectorLengthPrefixing,
                                      r.dynenv.caseSensitive):
                new PortValueWriter(port.getWriter(), r.dynenv.vectorLengthPrefixing,
                                    r.dynenv.caseSensitive);
            if (r.dynenv.customPrinting)
            	w=new CustomValueWriter(w,r.dynenv);
            	
            if (display) w.display(v);
            else w.write(v);
        } catch (IOException e) {
            throwIOException(r, liMessage(IOB, "errorwriting", 
                                          port.toString(),
                                          e.getMessage()), e);
        } catch (Exception e) {
            e.printStackTrace();
        }
        return VOID;
    }

    public static URL urlClean(URL u) {
        String encoding = "UTF-8";

        try {
            if (u.getProtocol().equals("file") &&
                (u.getRef()!=null || u.getQuery()!=null)) {
                StringBuffer b=new StringBuffer(u.getProtocol());
                b.append(':');
                b.append(u.getPath());
                if (u.getRef()!=null) {
                    b.append("%23");
                    b.append(URLEncoder.encode(u.getRef(), encoding));
                }
                if (u.getQuery()!=null) {
                    b.append("%3F");
                    b.append(URLEncoder.encode(u.getQuery(), encoding));
                }
                try {
                    u=new URL(b.toString());
                } catch (Exception e2) {
                    e2.printStackTrace();
                }
            } 
            return u;
        } catch (UnsupportedEncodingException use) {
            // We should probably use throwIOException, but we don't have
            // an interpreter at this point.
            Procedure.throwPrimException(
                liMessage(IO.IOB, "unsupencoding", encoding)
            );
            return null;    // not reached
        }
    }

    public static SchemeCharacterInputPort openCharInFile(Interpreter f,
                                                 URL u,
                                                 Charset encoding) 
        throws ContinuationException {    
        try {
            return new SchemeCharacterInputPort(
                    new SourceReader(new BufferedReader(encoding.newInputStreamReader(getURLInputStream(u))),
                                       u.toString()));
        } catch (IOException e) {
            throwIOException(f, liMessage(IOB, "erroropening", 
                                          u.toString()), e);
        }
        return null;
    }

    public static SchemeCharacterOutputPort openCharOutFile(Interpreter f, 
                                                   URL url,
                                                            Charset encoding)
        throws ContinuationException {
        try {
            Writer w=new OutputStreamWriter(getURLOutputStream(url), encoding.getCharsetName());           
            return new SchemeCharacterOutputPort(w);
        } catch (IOException e) {
            e.printStackTrace();
            throwIOException(f, liMessage(IOB, "erroropening",
                                          url.toString()), e);
        }
        return null;
    }
    
    public static InputStream getURLInputStream(URL u) throws IOException {
        URLConnection conn = u.openConnection();
        conn.setDoInput(true);
        conn.setDoOutput(false);
        return conn.getInputStream();
    }
 
    public static OutputStream getURLOutputStream(URL u) throws IOException {
        if (u.getProtocol().equals("file")) {
            //the JDK does not permit write access to file URLs
            return new FileOutputStream(u.getPath());
        }        
        URLConnection conn = u.openConnection();
        conn.setDoInput(false);
        conn.setDoOutput(true);
        return conn.getOutputStream();
    }
        
    public static void load(Interpreter f, URL u, boolean expanded)
        throws ContinuationException {

        SchemeCharacterInputPort p = null;
        SourceReader sr = null;
        try {
            URLConnection conn = u.openConnection();
            conn.setDoInput(true);
            conn.setDoOutput(false);
            // XXX possibly use conn.guessContentTypeFromStream(),
            // to get the stream's content-encoding
            sr=new SourceReader(new InputStreamReader(conn.getInputStream(),
                    Util.charsetFromString
                    (conn.getContentEncoding()).getCharsetName()), u.toString());
            p = new SchemeCharacterInputPort(sr);
        } catch (IOException e) {
            throwIOException(f, liMessage(IOB, "erroropening",
                                          u.toString()),
                             e);
        }

        Interpreter r = Context.enter(f.dynenv);

        try {
            Value v = null;
            do {
                int startLine = sr.line;
                int startColumn = sr.column;
                v = readCode(f, p);

                if (v != EOF) {
                    try {
                        if (expanded) {
                            r.interpret(r.compile(v));
                        } else {
                            r.eval(v, f.tpl);
                        }
                    } catch (SchemeException se) {
                        maybeThrowErrorWithExprLocation(se, v);
                        throwNestedPrimException(liMessage(IOB, "evalat", sr.sourceFile, startLine, startColumn), se);
                    }
                }
            } while (v != EOF);
        } finally {
            Context.exit();
        }
    }

    public Value doApply(Interpreter f)
        throws ContinuationException {
        switch (f.vlr.length) {
        case 0:
            switch (id) {
            case CHARREADY:
                try {
                    return SchemeBoolean.get(f.dynenv.getCurrentInReader().ready());
                } catch (IOException e) {
                    return FALSE;
                }
            case FLUSHOUTPUTPORT:
                try {
                    f.dynenv.getCurrentOutWriter().flush();
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorflushing", 
                                                  f.dynenv.out.toString()), e);
                }
                return VOID;
            case PEEKCHAR:
            	return peekChar(f, (SchemeCharacterInputPort) f.dynenv.getCurrentInPort());
            case PEEKBYTE:
            	return peekByte(f, (SchemeBinaryInputPort) f.dynenv.getCurrentInPort());
            case READ:
                return read(f, (SchemeCharacterInputPort) f.dynenv.getCurrentInPort());
            case READBYTE:
                return readByte(f, (SchemeBinaryInputPort) f.dynenv.getCurrentInPort());
            case READCHAR:
                return readChar(f, (SchemeCharacterInputPort) f.dynenv.getCurrentInPort());
            case READCODE:
                return readCode(f, (SchemeCharacterInputPort) f.dynenv.getCurrentInPort());
            default:
                throwArgSizeException();
            }
        case 1:
            switch (id) {
            case PORTQ: return SchemeBoolean.get(f.vlr[0] instanceof Port);
            case INPORTQ: return SchemeBoolean.get(f.vlr[0] instanceof InputPort);
            case OUTPORTQ: return SchemeBoolean.get(f.vlr[0] instanceof OutputPort);
            case CHARREADY:
                InputPort inport=(SchemeCharacterInputPort) f.vlr[0];
                try {
                    return SchemeBoolean.get(inport.ready());
                } catch (IOException e) {
                    return FALSE;
                }
            case DISPLAY:
                return displayOrWrite(f, (SchemeCharacterOutputPort) f.dynenv.getCurrentOutPort(), f.vlr[0], true);
            case WRITE:
                return displayOrWrite(f, (SchemeCharacterOutputPort) f.dynenv.getCurrentOutPort(), f.vlr[0], false);
            case PEEKBYTE:
                return peekByte(f, (SchemeBinaryInputPort) f.vlr[0]);
            case PEEKCHAR:
            	return peekChar(f, (SchemeCharacterInputPort) f.vlr[0]);
            case READ:
                SchemeCharacterInputPort cinport=(SchemeCharacterInputPort) f.vlr[0];
                return read(f, cinport);
            case READBYTE:
                SchemeBinaryInputPort binport=(SchemeBinaryInputPort) f.vlr[0];
                return readByte(f, binport);
            case READCHAR:
                cinport=(SchemeCharacterInputPort) f.vlr[0];
                return readChar(f, cinport);
            case READCODE:
                cinport=(SchemeCharacterInputPort) f.vlr[0];
                return readCode(f, cinport);
            case OPENCHARINPUTPORT:
            	return new SchemeCharacterInputPort(new PushbackReader(new BufferedReader(
            			f.dynenv.getCharacterSet().newInputStreamReader(((SchemeBinaryInputPort) f.vlr[0]).getInputStream()))));
            case OPENCHAROUTPUTPORT:
                return new SchemeCharacterOutputPort(new BufferedWriter(
                        f.dynenv.getCharacterSet().newOutputStreamWriter(((SchemeBinaryOutputPort) f.vlr[0]).getOutputStream())));
            case OPENSOURCEINPUTFILE:
                URL url = url(f.vlr[0]);
                return openCharInFile(f, url, f.dynenv.characterSet);
            case OPENINPUTFILE:
                url = url(f.vlr[0]);
                return openCharInFile(f, url, f.dynenv.characterSet);
            case OPENOUTPUTFILE:
                url = url(f.vlr[0]);
                return openCharOutFile(f, url, f.dynenv.characterSet);
            case OPENBUFFEREDCHARINPORT: 
            	return new SchemeCharacterInputPort(new BufferedReader(((SchemeCharacterInputPort) f.vlr[0]).getReader()));
            case OPENBUFFEREDCHAROUTPORT: 
            	return new SchemeCharacterOutputPort(new BufferedWriter(((SchemeCharacterOutputPort) f.vlr[0]).getWriter()));
            case FLUSHOUTPUTPORT:
                OutputPort op=(OutputPort) f.vlr[0];
                try {
                    op.flush();
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorflushing", 
                                                  op.toString()), e);
                }
                return VOID;
            case CLOSEINPUTPORT:
                InputPort inp=(InputPort) f.vlr[0];
                try {
                    if (inp!=f.dynenv.in) inp.close();
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorclosing",
                                                  inp.toString()),
                                     e);
                }
                return VOID;
            case CLOSEOUTPUTPORT:
                OutputPort outp=(OutputPort) f.vlr[0];
                try {
                    if (outp!=f.dynenv.out) outp.close();
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorclosing",
                                                  outp.toString()),
                                     e);
                }
                return VOID;
            case INPORTLOCATION:
                Reader in = ((SchemeCharacterInputPort) f.vlr[0]).getReader();
                if (in instanceof SourceReader) {
                    SourceReader sinp = (SourceReader)in;
                    return sourceAnnotations(sinp.sourceFile,
                                             sinp.line,
                                             sinp.column,
                                             f.dynenv.sourceAnnotations);
                } else
                    return FALSE;
            case LOAD:
                load(f, url(f.vlr[0]), false);
                return VOID;
            case LOADEXPANDED:
                load(f, url(f.vlr[0]), true);
                return VOID;
            case WRITECHAR:
                try {
                    f.dynenv.getCurrentOutWriter().write(SchemeCharacter.charValue(f.vlr[0]));
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorwriting",
                                                  f.dynenv.out.toString(),
                                                  e.getMessage()), e);
                }
                return VOID;
            case WRITEBYTE:
                try {
                    (((SchemeBinaryOutputPort) f.dynenv.getCurrentOutPort()).getOutputStream()).write(((Quantity) f.vlr[0]).indexValue());
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorwriting",
                                                  f.dynenv.out.toString(),
                                                  e.getMessage()), e);
                }
                return VOID;
            case FILEEXISTSQ:
                try {
                    url(f.vlr[0]).openConnection().getInputStream().close();
                    return TRUE;
                } catch (IOException e) {
                    return FALSE;
                }
            case FINDRESOURCE:
                url = Util.currentClassLoader().getResource(SchemeString.asString(f.vlr[0]));
                if (url == null) 
                    return FALSE;
                else return new SchemeString(url.toString());
            case FINDRESOURCES:
                java.util.Enumeration e;
                try {
                    e = Util.currentClassLoader().getResources(SchemeString.asString(f.vlr[0]));
                } catch (IOException ex) {
                    return EMPTYLIST;
                }
                if (!e.hasMoreElements()) return EMPTYLIST;
                Pair pa = new Pair();
                while(true) {
                    pa.setCar(new SchemeString((String)e.nextElement()));
                    if (!e.hasMoreElements()) break;
                    pa.setCdr(new Pair());
                    pa = (Pair)pa.cdr();
                }
                return pa;
            case ABSPATHQ:
                String f1=SchemeString.asString(f.vlr[0]);
                if (f1.startsWith("file:"))
                    f1=f1.substring(5);
                File fn=new File(f1);
                return SchemeBoolean.get(fn.isAbsolute());
            case NORMALIZEURL:
                URL u=urlClean(url(f.vlr[0]));
                return new SchemeString(u.toString());
            default:
                throwArgSizeException();
            }
        case 2:
            switch (id) {
            case WRITECHAR:
                Writer port=((SchemeCharacterOutputPort) f.vlr[1]).getWriter();
                try {
                    port.write(SchemeCharacter.charValue(f.vlr[0]));
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorwriting",
                                                  port.toString(),
                                                  e.getMessage()), e);
                }
                return VOID;
            case WRITEBYTE:
                OutputStream bport=((SchemeBinaryOutputPort) f.vlr[1]).getOutputStream();
                try {
                    bport.write(((Quantity) f.vlr[0]).indexValue());
                } catch (IOException e) {
                    throwIOException(f, liMessage(IOB, "errorwriting",
                                                  bport.toString(),
                                                  e.getMessage()), e);
                }
                return VOID;
            case DISPLAY:
                return displayOrWrite(f, (SchemeCharacterOutputPort) f.vlr[1], f.vlr[0], true);
            case WRITE:
                return displayOrWrite(f, (SchemeCharacterOutputPort) f.vlr[1], f.vlr[0], false);
            case OPENCHARINPUTPORT:
                try {
                    return new SchemeCharacterInputPort(new PushbackReader(new BufferedReader(
                            Charset.forName(SchemeString.asString(f.vlr[1])).newInputStreamReader(((SchemeBinaryInputPort) f.vlr[0]).getInputStream()))));
                } catch (UnsupportedEncodingException use) {
                    throwIOException(f, liMessage(IOB, "unsupencoding", SchemeString.asString(f.vlr[1])), 
                            new IOException(use.getMessage())); 
                }
            case OPENCHAROUTPUTPORT:
                try {
                    return new SchemeCharacterOutputPort(new BufferedWriter(
                            Charset.forName(SchemeString.asString(f.vlr[1])).newOutputStreamWriter(((SchemeBinaryOutputPort) f.vlr[0]).getOutputStream())));
                } catch (UnsupportedEncodingException use) {
                    throwIOException(f, liMessage(IOB, "unsupencoding", SchemeString.asString(f.vlr[1])), 
                            new IOException(use.getMessage())); 
                }
            case OPENINPUTFILE:
                URL url = url(f.vlr[0]);
                return openCharInFile(f, url,
                                      Util.charsetFromString(SchemeString.asString(f.vlr[1])));
            case OPENOUTPUTFILE:
                url = url(f.vlr[0]);
                Charset encoding=f.dynenv.characterSet;
                encoding=Util.charsetFromString(SchemeString.asString(f.vlr[1]));
                return openCharOutFile(f, url, encoding);
            case OPENBUFFEREDCHARINPORT: 
            	return new SchemeCharacterInputPort(new BufferedReader(((SchemeCharacterInputPort) f.vlr[0]).getReader(),
            			((Quantity) f.vlr[1]).indexValue()));
            case OPENBUFFEREDCHAROUTPORT: 
            	return new SchemeCharacterOutputPort(new BufferedWriter(((SchemeCharacterOutputPort) f.vlr[0]).getWriter(),
            			((Quantity) f.vlr[1]).indexValue()));
            case NORMALIZEURL:
                return new SchemeString(urlClean(url(f.vlr[0], 
                                                     f.vlr[1])).toString());
            default:
                throwArgSizeException();
            }
        case 3:
            switch (id) {
            case READSTRING:
                try {
                    int charsRead=((SchemeString) f.vlr[0]).readFromReader(f.dynenv.getCurrentInReader(),
                            ((Quantity) f.vlr[1]).intValue(),
                            ((Quantity) f.vlr[2]).intValue());
                    if (charsRead < 0) return EOF;
                    else return Quantity.valueOf(charsRead);
                } catch (IOException e) {
                    throwIOException(f, e.getMessage(), e);
                }
                return VOID;
            case WRITESTRING:
                try {
                    ((SchemeString) f.vlr[0]).writeToWriter(f.dynenv.getCurrentOutWriter(),
                                                            ((Quantity) f.vlr[1]).intValue(),
                                                            ((Quantity) f.vlr[2]).intValue());
                } catch (IOException e) {
                    throwIOException(f, e.getMessage(), e);
                }
                return VOID;
            case OPENOUTPUTFILE:
                URL url = url(f.vlr[0]);
                return openCharOutFile(f, url,
                                       Util.charsetFromString(SchemeString.asString(f.vlr[1])));
            default:
                throwArgSizeException();
            }
        case 4:
            switch (id) {
            case READSTRING:
                try {
                    int charsRead=((SchemeString) f.vlr[0]).readFromReader(((SchemeCharacterInputPort) f.vlr[3]).getReader(),
                            ((Quantity) f.vlr[1]).intValue(),
                            ((Quantity) f.vlr[2]).intValue());
                	if (charsRead<0) return EOF;
                	else return Quantity.valueOf(charsRead);
                } catch (IOException e) {
                    throwIOException(f, e.getMessage(), e);
                }
                return VOID;
            case WRITESTRING:
                try {
                    ((SchemeString) f.vlr[0]).writeToWriter(((SchemeCharacterOutputPort) f.vlr[3]).getWriter(),
                                                ((Quantity) f.vlr[1]).intValue(),
                                                ((Quantity) f.vlr[2]).intValue());
                } catch (IOException e) {
                    throwIOException(f, e.getMessage(), e);
                }
                return VOID;
            default:
                throwArgSizeException();
            }
        default:
            throwArgSizeException();
        }
        return VOID;
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
