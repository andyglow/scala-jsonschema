package com.github.andyglow.json;


import java.math.BigDecimal;
import java.math.BigInteger;

interface JsonHandler {

    void start();

    void end();

    void objectStart();

    void objectEnd();

    void arrayStart();

    void arrayEnd();

    void name(String name);

    void value(String value);

    void value(int value);

    void value(long value);

    void value(BigInteger value);

    void value(BigDecimal value);

    void value(boolean value);

    void nullValue();
}
