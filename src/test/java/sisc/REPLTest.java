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
import static org.hamcrest.Matchers.hasKey;

import org.hamcrest.core.IsInstanceOf;
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

	@Test
	public void abbreviationsAreExpanded() {
		String[] input = {"-c", "fry"};
		Map result = REPL.parseOpts(input);
		
		assertTrue(result.containsKey("call-with-args"));
		String mapValue = (String) result.get("call-with-args");
		assertThat(mapValue, equalTo("fry"));
	}
	
	@Test
	public void doubleOptionSyntaxWorks() {
		String[] input = {"--eval", "42"};
		Map result = REPL.parseOpts(input);
		
		assertTrue(result.containsKey("eval"));
		String mapValue = (String) result.get("eval");
		assertThat(mapValue, equalTo("42"));
	}
	
	@Test
	public void filesPassThrough() {
		String[] input = {"--eval", "42", "file1", "file2"};
		Map result = REPL.parseOpts(input);
		
		log.info("result was " + result);
		assertThat(result.get("files"), instanceOf(Vector.class));		
		Vector result2 = (Vector) result.get("files");
		String file1 = (String) result2.get(0);
		String file2 = (String) result2.get(1);
		
		assertThat(file1, equalTo("file1"));
		assertThat(file2, equalTo("file2"));
	}
	
	@Test
	public void switchesReturnBoolean() {
		String[] input = {"-v"};
		Map result = REPL.parseOpts(input);
		
		assertThat(result.get("version"), instanceOf(Boolean.class));
		Boolean value = (Boolean) result.get("version");
		assertTrue(value);
		
		log.info("result was " + result);
	}
	
	@Test
	public void allowsSeparatingSchemeArguments() {
		String[] input = {"file", "--", "argument-for-scheme", "another-one"};
		Map result = REPL.parseOpts(input);
		
		assertThat(result.get("argv"), instanceOf(Vector.class));
		Vector argv = (Vector) result.get("argv");
		
		assertThat((String) argv.get(0), equalTo("argument-for-scheme"));
		assertThat((String) argv.get(1), equalTo("another-one"));
		
		assertThat((String) ((Vector) result.get("files")).get(0), equalTo("file"));
	}	
}
