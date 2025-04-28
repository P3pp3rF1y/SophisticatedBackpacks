package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes;

import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.BackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;

public class BackpackSubtypeInterpreter extends PropertyBasedSubtypeInterpreter {
	public BackpackSubtypeInterpreter() {
		addProperty(s -> BackpackWrapper.fromStack(s).getMainColor(), "clothColor", String::valueOf);
		addProperty(s -> BackpackWrapper.fromStack(s).getAccentColor(), "borderColor", String::valueOf);
	}
}
