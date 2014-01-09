package sisc;

import java.util.Map;
import java.util.Vector;

import lombok.extern.slf4j.Slf4j;
import static org.junit.Assert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.hamcrest.collection.IsCollectionWithSize.hasSize;
import static org.hamcrest.core.IsInstanceOf.instanceOf;
import static org.hamcrest.core.IsNull.notNullValue;
import static org.hamcrest.CoreMatchers.equalTo; 
import static org.hamcrest.CoreMatchers.equalTo; 

import org.junit.Test;

@Slf4j
public class REPLTest {
	@Test
	public void testEmptyArguments() {
		String[] input = {};
		Map result = REPL.parseOpts(input);
		
		log.info("Result was: " + result);
		assertThat(result.size(), equalTo(1));
		assertThat(result.get("files"), instanceOf(Vector.class));
		
		Vector result2 = (Vector) result.get("files");
		assertTrue(result2.isEmpty());
	}
}
