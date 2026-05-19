package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes;

import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;

public class BackpackSubtypeInterpreter extends PropertyBasedSubtypeInterpreter {
	public BackpackSubtypeInterpreter() {
		addProperty(BackpackItem::getMainColor, "clothColor", String::valueOf);
		addProperty(BackpackItem::getAccentColor, "borderColor", String::valueOf);
	}
}
