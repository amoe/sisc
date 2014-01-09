package sisc.modules.s2j.dynclass;

import java.lang.reflect.Modifier;

import org.objectweb.asm.ClassWriter;
import org.objectweb.asm.Type;
import org.objectweb.asm.commons.GeneratorAdapter;
import org.objectweb.asm.commons.Method;



public class ConstructorSignature extends MethodSignature {
    
    int[] superArgs;
    
    public ConstructorSignature(int modifiers, Class[] parameterTypes,
                                Class[] exceptionTypes, int[] superArgs) {
        super(modifiers, Void.TYPE, "<init>", parameterTypes, exceptionTypes);
        this.superArgs=superArgs;
    
    }
    
    public void visitMethod(int procid, Type thisType, GeneratorAdapter mg, Type superType) {
        if (superArgs != null) {
            Type[] superTypes=new Type[superArgs.length];
            for (int i=0; i<superArgs.length; i++) {
                superTypes[i]=parameterTypes[superArgs[i]];
                mg.loadArg(superArgs[i]);
            }
            Method superMethod=new Method("<init>", Type.VOID_TYPE, superTypes);            
            mg.loadThis();
            mg.invokeConstructor(superType, superMethod);
        } else {
            Method superMethod=new Method("<init>", Type.VOID_TYPE, new Type[0]);
            mg.loadThis();
            mg.invokeConstructor(superType, superMethod);
        }
        super.visitMethod(procid, thisType, mg, superType);
    }

}
