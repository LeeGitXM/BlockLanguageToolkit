package com.ils.blt.common.serializable;

import java.lang.reflect.Type;

import org.joda.time.DateTime;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;


/**
 * Implement a plain-old-java-object representing a model diagram
 * that is serializable via the Ignition XML serializer.
 * 
 * The POJO objects should have no behavior.
 */
public class SerializableDiagram {
	private SerializableBlock[] blocks;
	private SerializableConnection[] connections;
	private String name;
	
	public SerializableDiagram() {	
		blocks = new SerializableBlock[0];
		connections= new SerializableConnection[0];
		name="UNSET";
	}
	
	public SerializableBlock[] getBlocks() { return blocks; }
	public SerializableConnection[] getConnections() { return connections; }
	public String getName() { return name; }

	public void setBlocks(SerializableBlock[] list) { blocks=list; }
	public void setConnections(SerializableConnection[] list) { connections=list; }
	public void setName(String nam) { name=nam; }
	
	private class DiagramSerializer implements JsonSerializer<SerializableDiagram> {
		public JsonElement serialize(SerializableDiagram src, Type typeOfSrc, JsonSerializationContext context) {
			return new JsonPrimitive(src.toString());
		}
	}
	private class DiagramDeserializer implements JsonDeserializer<SerializableDiagram> {
		public SerializableDiagram deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context)
				throws JsonParseException {
			return new SerializableDiagram(json.getAsJsonPrimitive().getAsString());
		}
	}
}
