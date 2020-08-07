/**
 * Copyright 2016. ILS Automation. All rights reserved.
 */
package com.ils.common.component.recmap;

public class NameValuePair {
    private final String name;
    private final String value;

    public NameValuePair(String key, String val) {
        this.name = key;
        this.value = val;
    }

    public String getName() {return this.name;}

    public String getValue() {return this.value;}
}
