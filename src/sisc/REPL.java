package sisc;

import java.io.*;
import java.util.*;
import java.net.*;
import sisc.data.*;
import sisc.interpreter.*;
import sisc.env.DynamicEnvironment;
import sisc.util.Util;

/**
 * Entry point for the SISC Read Eval Print Loop.  Responsible for
 * command line parsing and invoking the REPL Scheme code
 */
public class REPL {

    public SchemeThread primordialThread;

    /**
     * Construct a new REPL which will invoke the given 
     * Scheme procedure as the REPL in the provided dynamic environment.
     *
     * @param dynenv The dynamic environment for the REPL
     * @param repl The procedural entry-point of the REPL
     */
    public REPL(DynamicEnvironment dynenv, Procedure repl) {
        this(new SchemeThread(dynenv, repl));
    }

    public REPL(SchemeThread primordialThread) {
        this.primordialThread = primordialThread;
        primordialThread.thread.setDaemon(true);
    }
    
    public void go() {
        if (primordialThread.thunk == null) {
            System.err.println(Util.liMessage(Util.SISCB, "heapnotfound"));
            return;
        }

        primordialThread.start();
    }

    public static Procedure getCliProc(AppContext ctx) {
        return (Procedure)ctx.toplevel_env.lookup(Symbol.get("sisc-cli"));
    }

    public static void main(String[] argv) throws Exception {
        Map args=parseOpts(argv);

        if (args.get("help")!=null) {
            printUsage();
            System.exit(0);
        } else if (args.get("version")!=null) {
            System.out.println("SISC - The Second Interpreter of Scheme Code - "+
                               Util.VERSION);
            System.exit(0);
        }

        Properties props = new Properties();
        String configFile = (String)args.get("properties");
        if (configFile != null) {
            try {
                URL url = Util.url(configFile);
                URLConnection conn = url.openConnection();
                conn.setDoInput(true);
                conn.setDoOutput(false);
                props.load(conn.getInputStream());
            } catch (MalformedURLException e) {
                System.err.println("WARNING: " + e.getMessage());
            } catch (IOException e) {
                System.err.println("WARNING: " + e.getMessage());
            }
        }

        AppContext ctx = new AppContext(props);
        Context.setDefaultAppContext(ctx);

        URL heap = AppContext.findHeap(Util.makeURL((String)args.get("heap")));
        if (heap==null) {
            System.err.println(Util.liMessage(Util.SISCB, "heapnotfound"));
            return;
        }
        if (!ctx.addHeap(AppContext.openHeap(heap))) 
            return;

        Interpreter r = Context.enter(ctx);

        boolean filesLoadedSuccessfully = 
            r.loadSourceFiles((String[])((Vector)args.get("files")).toArray(new String[0]));
            
        boolean noRepl=args.get("no-repl")!=null;
        boolean call=args.get("call-with-args")!=null;
        int returnCode = 0;

        String expr=(String)args.get("eval");
        if (expr!=null) {
            Value v=Util.VOID;
            try {
                v=r.eval(expr);
                if (!call) 
                    System.out.println(v);
            } catch (SchemeException se) {
                se.printStackTrace();
                returnCode = 1;
            }
        }

        String func=(String)args.get("call-with-args");
        if (func!=null) {
            Procedure fun=null;
            try {
                fun=Util.proc(r.eval(func));
            } catch (SchemeException se) {
                se.printStackTrace();
                returnCode = 1;
            }
            if (fun!=null) {
                Vector av=(Vector)args.get("argv");
                Value[] sargs=new Value[(av == null ? 0 : 
                                         av.size())];
                for (int i=0; i<sargs.length; i++) 
                    sargs[i]=new SchemeString((String)av.elementAt(i));
                Value v=Util.VOID;
                try {
                    v=r.eval(fun, sargs);
                    if (noRepl) {
                        if (v instanceof Quantity)
                            returnCode=((Quantity)v).indexValue();
                        else if (!(v instanceof SchemeVoid)) {
                            System.out.println(v);
                        }
                    }
                } catch (SchemeException se) {
                    se.printStackTrace();
                    returnCode = 1;
                }           
            }
        }
        
        DynamicEnvironment dynenv=r.dynenv;
        Context.exit();
        
        if (!noRepl) {
            String listen = (String)args.get("listen");
            if (listen!=null) {
                int cidx = listen.indexOf(':');
                ServerSocket ssocket = cidx == -1 ?
                    new ServerSocket(Integer.parseInt(listen), 50) :
                    new ServerSocket(Integer.parseInt(listen.substring(cidx+1)), 50, 
                                     InetAddress.getByName(listen.substring(0, cidx)));
                System.out.println("Listening on " + ssocket.getInetAddress().toString() + ":" + ssocket.getLocalPort());
                System.out.flush();
                listen(ctx, ssocket);
            } else {
                REPL repl = new REPL(dynenv, getCliProc(ctx));
                repl.go();
                repl.primordialThread.thread.join();
                switch (repl.primordialThread.state) {
                case SchemeThread.FINISHED:
                    if (repl.primordialThread.rv instanceof Quantity) {
                        returnCode=((Quantity)repl.primordialThread.rv).intValue();
                    }
                    break;
                case SchemeThread.FINISHED_ABNORMALLY:
                    returnCode=1;
                    break;
                }                                    
            }
        } else if (returnCode == 0 && !filesLoadedSuccessfully) {
            returnCode=1;
        }

        System.exit(returnCode);
    }

    public static void listen(AppContext ctx, ServerSocket ssocket)
        throws IOException {
        for (;;) {
            Socket client = ssocket.accept();
            DynamicEnvironment dynenv =
                new DynamicEnvironment(ctx,
                                       client.getInputStream(),
                                       client.getOutputStream());
            SchemeThread t = new SchemeSocketThread(dynenv,
                                                    getCliProc(ctx),
                                                    client);
            REPL repl = new REPL(t);
            repl.go();
        }
    }
    
    public static class SchemeSocketThread extends SchemeThread {

        public Socket s;
        
        public SchemeSocketThread(DynamicEnvironment dynenv,
                                  Procedure thunk,
                                  Socket s) {
            super(dynenv, thunk);
            this.s = s;
        }

        public void run() {
            super.run();
            try {
                s.close();
            } catch(IOException e) {}
        }

    }

    public static void printUsage() {
        System.out.println("SISC - Second Interpreter of Scheme Code\n");
        System.out.println("usage: sisc [-?xv] [-l port] [-h heapfile] [-p property-file]");
        System.out.println("            [-e s-expression] [-c function] [source-file ...] [-- arguments ...]");
    }

    static final int SWITCH=0, OPTION=1;
    static final String[][] opts=new String[][] {
        {"?","help"},
        {"l","listen"},
        {"h","heap"},
        {"p","properties"},
        {"e","eval"},
        {"c","call-with-args"},
        {"x","no-repl"},
        {"v","version"}
    };
    static final int optTypes[]=new int[] {
        SWITCH, OPTION, OPTION, OPTION, OPTION, OPTION, SWITCH, SWITCH};
                                                  
    public static Map parseOpts(String[] args) {
        Map m=new HashMap();
        Vector files=new Vector();
        int x=0;
        while (x<args.length) {
            String a=args[x];
            int y;
            if (a.startsWith("-")) {
                if (a.startsWith("--")) 
                    y=1;
                else 
                    y=0;
                a=a.substring(y+1);
                if ("".equals(a)) {
                    Vector params=new Vector();
                    if (y==0) 
                        params.addElement("-");
                    else {
                        while (++x<args.length) {
                            params.addElement(args[x]);
                        }
                    }
                    m.put("argv", params);
                    break;
                } else {
                    for (int i=0; i<opts.length; i++) {
                        if (opts[i][y].equals(a)) {
                            switch (optTypes[i]) {
                            case OPTION:
                                m.put(opts[i][1], args[++x]);
                                break;
                            case SWITCH:
                                m.put(opts[i][1], Boolean.TRUE);
                                break;
                            }
                            break;
                        }
                    }
                }
            } else {
                files.addElement(args[x]);
            }
            x++;
        }
        m.put("files", files);
        return m;
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
