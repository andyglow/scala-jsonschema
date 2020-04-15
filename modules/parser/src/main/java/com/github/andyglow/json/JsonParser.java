package com.github.andyglow.json;

import java.io.*;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayDeque;
import java.util.function.BiFunction;
import java.util.function.Consumer;
import java.util.function.Function;


class JsonParser {

    private enum State {
        JSON,
        OBJECT_BEFORE_COLON,
        OBJECT_AFTER_COLON,
        ARRAY,
    }

    private InputStream is;

    private JsonHandler handler;

    JsonParser(String input, JsonHandler handler) {
        this(new ByteArrayInputStream(input.getBytes()), handler);
    }

    JsonParser(InputStream is, JsonHandler handler) {
        this.is = is;
        this.handler = handler;
    }

    public void parse() throws IOException, JsonParseException {
        try (
            Reader _reader = new InputStreamReader(is);
            PushbackReader reader = new PushbackReader(_reader)) {

            int r;
            ArrayDeque<State> state = new ArrayDeque<>();
            state.push(State.JSON);
            handler.start();
            while ((r = reader.read()) != -1) {
                char c = (char) r;
                switch(c) {
                    case '{': {
                        if (state.peek() == State.OBJECT_BEFORE_COLON) throw new JsonParseException();

                        state.push(State.OBJECT_BEFORE_COLON);
                        handler.objectStart();

                        break;
                    }
                    case '}': {

                        state.pop();
                        if (state.peek() == State.OBJECT_BEFORE_COLON) state.pop();
                        handler.objectEnd();

                        break;
                    }

                    case '[': {
                        state.push(State.ARRAY);
                        handler.arrayStart();

                        break;
                    }
                    case ']': {
                        if (state.peek() != State.ARRAY) throw new JsonParseException();

                        handler.arrayEnd();
                        state.pop();

                        break;
                    }

                    case 'f': {
                        if (state.peek() == State.OBJECT_BEFORE_COLON) throw new JsonParseException();

                        tryParse(reader, "alse", () -> handler.value(false));


                        break;
                    }
                    case 't': {
                        if (state.peek() == State.OBJECT_BEFORE_COLON) throw new JsonParseException();

                        tryParse(reader, "rue", () -> handler.value(true));

                        break;
                    }
                    case 'n': {
                        if (state.peek() == State.OBJECT_BEFORE_COLON) throw new JsonParseException();

                        tryParse(reader, "ull", () -> handler.nullValue());

                        break;
                    }

                    case '-':
                    case '.':
                    case '0':
                    case '1':
                    case '2':
                    case '3':
                    case '4':
                    case '5':
                    case '6':
                    case '7':
                    case '8':
                    case '9':  {
                        if (state.peek() == State.OBJECT_BEFORE_COLON) throw new JsonParseException();
                        reader.unread(c);
                        tryParseNumber(reader, (sb) -> (dotExists) -> (exponentExists) -> {
                            String text = sb.toString();
                            int len = text.length();
                            if (dotExists) {
                                handler.value(new BigDecimal(text));
                            } else {
                                if (exponentExists || len > 18)
                                    handler.value(new BigDecimal(text).toBigInteger());
                                else if (len <= 9)
                                    handler.value(Integer.parseInt(text));
                                else
                                    handler.value(Long.parseLong(text));
                            }

                            return null;
                        });

                        break;
                    }

                    case '"': {
                        tryParseString(reader, (text) -> {
                            if (state.peek() == State.OBJECT_BEFORE_COLON) {
                                handler.name(text.toString());

                            } else {
                                handler.value(text.toString());
                            }
                        });

                        break;
                    }

                    case ':': {
                        if (state.peek() == State.OBJECT_AFTER_COLON) throw new JsonParseException();

                        state.push(State.OBJECT_AFTER_COLON);

                        break;
                    }

                    case ',': {
                        if (state.peek() == State.ARRAY) break;

                        if (state.peek() == State.OBJECT_BEFORE_COLON) throw new JsonParseException();
                        state.pop();

                        break;
                    }

                    default:
                        if (!Character.isWhitespace(c))
                            throw new JsonParseException("Unexpected [" + c + "]");
                }
            }
            if (state.peek() != State.JSON) throw new JsonParseException();

            handler.end();
        }
    }

    private static void tryParse(PushbackReader reader, String expecting, Runnable cb) throws IOException, JsonParseException {
        int len = expecting.length();
        char[] buf = new char[len];
        if (reader.read(buf, 0, len) == expecting.length()) {
            if (new String(buf).equals(expecting)) cb.run(); else throw new JsonParseException();
        } else {
            throw new JsonParseException();
        }
    }

    private static void tryParseNumber(PushbackReader reader, Function<StringBuilder, Function<Boolean, Function<Boolean, Void>>> cb) throws IOException, JsonParseException {
        StringBuilder sb = new StringBuilder();
        boolean dotExists = false;
        boolean minusExists = false;
        boolean exponentExists = false;
        int i;
        char prev = (char) -1;
        while ((i = reader.read()) != -1) {
            char c = (char) i;
            if (c == 'e') {
                if (exponentExists) throw new JsonParseException();
                else {
                    sb.append(c);
                    exponentExists = true;
                }
            } else if (Character.isDigit(c)) {
                sb.append(c);
            } else if (c == '+') {
                if (prev == 'e') {
                    sb.append(c);
                } else
                    throw new JsonParseException();
            } else if (c == '-') {
                if (!minusExists || prev == 'e') {
                    sb.append(c);
                    minusExists = true;
                } else throw new JsonParseException();
            } else if (c == '.') {
                if (!dotExists) {
                    sb.append(c);
                    dotExists = true;
                } else
                    throw new JsonParseException();
            } else {
                cb.apply(sb).apply(dotExists).apply(exponentExists);
                reader.unread(i);
                return;
            }
            prev = c;
        }

        cb.apply(sb).apply(dotExists).apply(exponentExists);
    }

    private static void tryParseString(PushbackReader reader, Consumer<StringBuilder> cb) throws IOException, JsonParseException {
        StringBuilder sb = new StringBuilder();
        int i;
        boolean escaped = false;
        while ((i = reader.read()) != -1) {
            char c = (char) i;
            if (c == '\\') {
                if (escaped) {
                    sb.append('\\');
                    escaped = false;
                } else {
                    escaped = true;
                }
            } else if (c != '"') {
                if (Character.isISOControl(c)) {
                    if (escaped) sb.append(c);
                    else throw new JsonParseException();
                } else
                    sb.append(c);
                escaped = false;
            } else {
                if (escaped) {
                    sb.append('"');
                } else {
                    cb.accept(sb);
                    return;
                }
                escaped = false;
            }
        }
        throw new JsonParseException();
    }
}