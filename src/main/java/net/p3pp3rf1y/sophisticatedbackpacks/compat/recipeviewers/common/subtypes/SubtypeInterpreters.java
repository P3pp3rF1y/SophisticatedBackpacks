package net.p3pp3rf1y.sophisticatedbackpacks.compat.recipeviewers.common.subtypes;

import net.minecraft.world.item.Item;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackItem;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModItems;
import net.p3pp3rf1y.sophisticatedcore.compat.recipeviewers.common.subtypes.PropertyBasedSubtypeInterpreter;

import java.util.HashMap;
import java.util.Map;

public class SubtypeInterpreters {
	private static final PropertyBasedSubtypeInterpreter backpackSubtypeInterpreter = new BackpackSubtypeInterpreter();

	public static Map<Item, PropertyBasedSubtypeInterpreter> getSubtypeInterpreters() {
		return new HashMap<>(){{
			ModItems.ITEMS.getEntries().stream()
					.filter(holder -> holder.get() instanceof BackpackItem)
					.forEach(item -> put(item.get(), backpackSubtypeInterpreter));
		}};
	}
}
