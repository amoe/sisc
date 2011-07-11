package sisc.modules.s2j.dynclass;

import java.lang.reflect.Modifier;

import org.objectweb.asm.ClassVisitor;
import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Label;
import org.objectweb.asm.MethodVisitor;
import org.objectweb.asm.Opcodes;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.commons.Method;

import sisc.data.Procedure;

public class MethodSignature implements Opcodes {

    static Class[] ZC=new Class[0];
    static Type methodProxyType=Type.getType(MethodProxy.class);
    static Type procedureType=Type.getType(SchemeHook.class);
    static Type procedureArrayType=Type.getType("[Lsisc/modules/s2j/dynclass/SchemeHook;");
    static Type objectType=Type.getType(Object.class);
    static Method methodProxyInvoke=new Method("invoke", objectType,
                                        new Type[] { procedureType,
                                                     objectType,
                                                     Type.getType("[Ljava/lang/Object;") });
    static Method methodProxyInvokeStatic=new Method("invoke", Type.getType(Object.class),
            new Type[] { procedureType,
                         Type.getType("[Ljava/lang/Object;") });

    int modifiers;
    String methodName;
    Type returnType;
    Type[] parameterTypes;
    Type[] exceptionTypes;
    
    public MethodSignature(int modifiers, Class returnType, String methodName, Class[] parameterTypes,
            Class[] exceptionTypes) {
        this.modifiers=modifiers;
        this.returnType=Type.getType(returnType);
        this.methodName=methodName;
        if (parameterTypes!=null) {
            this.parameterTypes=new Type[parameterTypes.length];
            for (int i=0; i<parameterTypes.length; i++) {
                this.parameterTypes[i]=Type.getType(parameterTypes[i]);
            }
        } else this.parameterTypes=new Type[0];
        if (exceptionTypes!=null) {
            this.exceptionTypes=new Type[exceptionTypes.length];
            for (int i=0; i<exceptionTypes.length; i++) {
                this.exceptionTypes[i]=Type.getType(exceptionTypes[i]);
            }
        }
        
    }
    
    Procedure[] __procs;
    
    void visitMethod(int procid, Type thisType, ClassWriter cw, Type superclassType) {       
        Method m=new Method(methodName, returnType, parameterTypes);
        GeneratorAdapter mg = new GeneratorAdapter(ClassSignature.asmModifiers(modifiers), m, null, exceptionTypes, cw);
        visitMethod(procid, thisType, mg, superclassType);
    }
    
    void visitMethod(int procid, Type thisType, GeneratorAdapter mg, Type superclassType) {        
        mg.getStatic(thisType, "__procs", procedureArrayType);
        mg.push(procid);
        mg.arrayLoad(procedureType);
        
        if ((modifiers & Modifier.STATIC) > 0) {
            mg.loadArgArray();
            mg.invokeStatic(methodProxyType, methodProxyInvokeStatic);
        } else {
            mg.loadThis();
            mg.loadArgArray(); 
            mg.invokeStatic(methodProxyType, methodProxyInvoke);
        }
        if (returnType!=Type.VOID_TYPE) {
            mg.checkCast(returnType);
        } 
        mg.returnValue();
        mg.endMethod();
    }

}
