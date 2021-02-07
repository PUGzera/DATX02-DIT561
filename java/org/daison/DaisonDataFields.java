package org.daison;

import java.lang.annotation.*;

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface DaisonDataFields {
	String[] fields();
}
