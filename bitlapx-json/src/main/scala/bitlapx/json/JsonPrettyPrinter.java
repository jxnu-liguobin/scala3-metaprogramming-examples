/*
 * Copyright (c) 2023 bitlap
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of
 * this software and associated documentation files (the "Software"), to deal in
 * the Software without restriction, including without limitation the rights to
 * use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
 * the Software, and to permit persons to whom the Software is furnished to do so,
 * subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
 * FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
 * COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
 * IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package bitlapx.json;

/**
 * This class contains methods to format JSON-Strings into a "pretty-printed"
 * more human-readable format.
 * @author lukasschmelzeisen
 */
public class JsonPrettyPrinter {
    
    /**
     * Amount of spaces inserted at each indentation level.
     */
    final public static int tabWidth = 4;

    /**
     * Repeats a `String` a number of `times`.
     * @param str    The string to be repeated.
     * @param times  Number of times `str` should be repeated.
     * @return Returns the repeated string. 
     */
    private static String strrepeat(String str, int times) {
        assert(times >= 0);
        
        String result = "";
        for (int i = 0; i != times; ++i) {
            result += str;
        }
        return result;
    }
    
    /**
     * Formats a JSON-String into a "pretty-printed" more human readable format.
     * @param json  The JSON-String to be formatted. This needs to be a minimal
     *              JSON, meaning no whitespace or newlines inside it.
     * @return The formatted JSON-String.
     */
    public static String prettyPrintJson(String json) {
        int indent = 0;
        boolean inString = false;
        boolean inEscape = false;
        
        String result = "";
        
        // We could change this method of iterating over string by using
        // reflection, this would improve performance for strings longer than
        // 500 chars. See:
        // http://stackoverflow.com/questions/8894258/fastest-way-to-iterate-over-all-the-chars-in-a-string
        for (char c : json.toCharArray()) {
            switch (c) {
                case '}':
                case ']':
                    if (!inString) {
                        --indent;
                        result += "\n" + strrepeat(" ", indent * tabWidth);
                    }
                    break;
            }
            
            result += c;
            
            switch (c) {
                case '{':
                case '[':
                    if (!inString) {
                        ++indent;
                        result += "\n" + strrepeat(" ", indent * tabWidth);
                    }
                    break;
                   
                case ':':
                    if (!inString)
                        result += " ";
                    break;
                    
                case ',':
                    if (!inString)
                        result += "\n" + strrepeat(" ", (indent * tabWidth) - 1); // -1 indent 
                    break;
                    
                case '"':
                    if (!inEscape)
                        inString = !inString;
                    break;
            }
            
            if (inEscape)
                inEscape = false;
            else if (c == '\\')
                inEscape = true;
        }
        
        return result;
    }

}