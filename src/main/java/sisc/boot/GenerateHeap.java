package sisc.boot;

import java.io.*;
import java.util.*;
import sisc.interpreter.*;
import sisc.ser.*;
import sisc.data.*;
import sisc.env.*;
import sisc.util.Util;
import sisc.util.Defaults;
import sisc.modules.R5RS;

public class GenerateHeap {

    static SymbolicEnvironment[] classify(SymbolicEnvironment base, 
                                          LibraryBuilder lb) {
        SymbolicEnvironment[] rv=new SymbolicEnvironment[2];
        rv[0]=new LibraryAE((Symbol)null, lb);
        rv[1]=base;
        // Now move the syntax
        ((MemorySymEnv)rv[0]).sidecars=((MemorySymEnv)base).sidecars;
        ((MemorySymEnv)base).sidecars=new HashMap();

        rv[1].setParent(rv[0]);

        HashMap r5rs=new HashMap();

        //Note the  R5RS bindings so we can transfer them to the
        //bottommost env.
        for (int i = 0; i < R5RS.bindingNames.length; i++) {
            Symbol key = R5RS.bindingNames[i];
            Value v = base.lookup(key);
            if (v == null) {
                System.out.println("WARNING: no binding for R5RS symbol " + key.symval);
            } else {
                r5rs.put(key, v);
                rv[0].define(key, v);
                base.undefine(key);
            }
        }

        return rv;
    }

    private static void patchDefaults() {
        //Annotations can only be emitted once we have an expander
        //installed. Things break horribly otherwise.
        Defaults.EMIT_ANNOTATIONS = false;
    }

    public static void main(String[] args) throws Exception {
        String inHeap = null;
        String outHeap = null;
        int i;

        for (i = 0; i < args.length; i++) {
          if ("-in".equalsIgnoreCase(args[i]))
            inHeap = args[++i];
          else if ("-out".equalsIgnoreCase(args[i]))
            outHeap = args[++i];
          else if ("-files".equalsIgnoreCase(args[i])) {
            i++;
            break;
          }
        }

        if (outHeap == null) {
          System.out.println("Output heap file name has not been specified!");
          System.exit(1);
        }

        patchDefaults();

        LibraryBuilder lb=new LibraryBuilder();
        MemorySymEnv symenv=new LibraryAE(Symbol.get("symenv"), lb);
        MemorySymEnv toplevel=new LibraryAE((Symbol)null, lb);
        toplevel.setName(Util.TOPLEVEL);
        sisc.compiler.Compiler.addSpecialForms(toplevel);
        symenv.define(Util.TOPLEVEL, toplevel);
        //we do the following so that code can explictly refer to the r5rs env during boot
        symenv.define(Util.REPORT, toplevel);

        // Set this initially, so the optimizer can do its core forms
        // check without error
        symenv.define(Util.SISC_SPECIFIC, toplevel);
        AppContext ctx = new AppContext(symenv);
        Context.setDefaultAppContext(ctx);
        lb.setAppContext(ctx);
        
        Interpreter r = Context.enter(ctx);
        new sisc.modules.Primitives.Index().bindAll(r, ctx.toplevel_env);
        new sisc.modules.Annotations.Index().bindAll(r, ctx.toplevel_env);
        new sisc.modules.io.IO.Index().bindAll(r, ctx.toplevel_env);
        new sisc.modules.io.StringIO.Index().bindAll(r, ctx.toplevel_env);
        
        Symbol loadExpSymb = Symbol.get("load-expanded");
        Symbol loadSymb = Symbol.get("load");
        
        if (inHeap != null) {
            System.out.println("Reading input heap: " + inHeap);
            ctx.loadEnv(new SeekableDataInputStream(new BufferedRandomAccessInputStream(inHeap, "r", 1, 8192)));
        }
        
        r.define(Symbol.get("version"), new SchemeString(Util.VERSION), Util.SISC);

        System.out.println("Generating heap: "+outHeap);

        Procedure load=null;
        for (; i<args.length; i++) {
            System.out.println("Expanding and compiling "+args[i]+"...");
            try {
                if (args[i].endsWith("sce"))
                    load=(Procedure)r.getCtx().toplevel_env.lookup(loadExpSymb);
                else
                    load=(Procedure)r.getCtx().toplevel_env.lookup(loadSymb);
                r.eval(load, new Value[] {new SchemeString(args[i])});
            } catch (SchemeException se) {
                System.err.println("Error during expand: "+se.getMessage());
                try {
                    r.eval((Procedure)
                           r.eval(Symbol.get("print-exception")), 
                           new Value[] {se.m});
                } catch (Exception e) {
                    e.printStackTrace();
                    // Oh well, we tried
                }
            }
        }
        
        System.err.println("Partitioning bindings...");
        SymbolicEnvironment[] results=classify(r.lookupContextEnv(Util.TOPLEVEL), lb);
        SymbolicEnvironment sisc_specific, r5rs, top_level;
        r5rs=results[0];
        sisc_specific=results[1];
        sisc_specific=new LibraryAE(sisc_specific, lb);
        sisc_specific.setName(Util.SISC_SPECIFIC);
        
        r.getCtx().toplevel_env=top_level=new MemorySymEnv(sisc_specific, Util.TOPLEVEL);
        r5rs.setName(Util.REPORT);
        sisc_specific.setName(Util.SISC_SPECIFIC);
        
        toplevel.setName(Symbol.get("*toplevel-lib*"));
        r.defineContextEnv(Util.TOPLEVEL, top_level);
        r.defineContextEnv(Util.REPORT, r5rs);
        r.defineContextEnv(Util.SISC_SPECIFIC, sisc_specific);


        System.out.println("Saving heap...");

        try {
            OutputStream out= //new GZIPOutputStream(
                                 new BufferedOutputStream(
                                    new FileOutputStream(outHeap));
            ctx.saveEnv(out,lb);
            out.flush();
            out.close();
        } catch (Exception e) {
            System.err.println("Error generating heap:");
            e.printStackTrace();
            System.exit(1);

        }
        System.out.println("Heap saved.");
               Context.exit();
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
