package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes;

import net.p3pp3rf1y.sophisticatedbackpacks.api.CapabilityBackpackWrapper;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;
import net.p3pp3rf1y.sophisticatedcore.util.ITintable;

public class BackpackSubtypeInterpreter extends PropertyBasedSubtypeInterpreter {
	public BackpackSubtypeInterpreter() {
		addProperty(s -> s.getCapability(CapabilityBackpackWrapper.BACKPACK_WRAPPER_CAPABILITY).map(ITintable::getMainColor).orElse(-1), "clothColor", String::valueOf);
		addProperty(s -> s.getCapability(CapabilityBackpackWrapper.BACKPACK_WRAPPER_CAPABILITY).map(ITintable::getAccentColor).orElse(-1), "borderColor", String::valueOf);
	}
}
