import sisc.interpreter.*;
import java.io.IOException;

/**
 * An example program demonstrating calling into SISC, assuming
 * the classpath and SISC_HOME point to the appropriate SISC jars
 * and the heap, respectively.
 */
public class SchemeHelloWorld {

    public static void main(String[] args) throws SchemeException {
        Context.execute(new SchemeCaller() {
                public Object execute(Interpreter r) throws SchemeException {
                    try {
                        return r.eval("(begin (display \"Hello, World!\") (newline))");
                    } catch (IOException e) {
                        // Thrown if the given Scheme program cannot be parsed
                        return null;
                    }
                }
            });
    }
}

                        
