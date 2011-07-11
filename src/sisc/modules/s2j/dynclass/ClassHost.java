package sisc.modules.s2j.dynclass;

import java.lang.reflect.Field;
import java.lang.reflect.Modifier;
import java.util.WeakHashMap;

import sisc.data.Procedure;

public class ClassHost extends ClassLoader {

    static WeakHashMap classHooks=new WeakHashMap();
    
    public Class newClass(String className, byte[] bytecode, SchemeHook[] procs) {
        Class c=defineClass(className, bytecode, 0, bytecode.length);
        try {
            Field f = c.getField("__procs");
            f.set(null, procs);
            f=c.getField("__bytecode");
            f.set(null, bytecode);
        } catch (SecurityException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (NoSuchFieldException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IllegalArgumentException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        } catch (IllegalAccessException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        return c;        
    }
    
    public Class newClass(ClassSignature sig, SchemeHook[] procs) {
        byte[] bc=sig.generateBytecode();
        return newClass(sig.className, bc, procs);
    }
    
    public static Procedure[] getClassHooks(Class c) {
        return (Procedure[])classHooks.get(c);
    }
    
    public static void main(String[] args) throws Exception {
        //Hello world example
        MethodSignature test=new MethodSignature(Modifier.PUBLIC, Void.TYPE, "test", null, null);
        ClassSignature testclass=new ClassSignature(Modifier.PUBLIC, "HelloWorld", null, null,
                null,
                new MethodSignature[] {test});
        ClassHost ch=new ClassHost();
        Class hwClass=ch.newClass(testclass, null);
        Object hwi=hwClass.newInstance();
        hwClass.getMethods()[0].invoke(hwi, new Object[0]);
    }
}
