package net.p3pp3rf1y.sophisticatedcore.upgrades.stack;

import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.item.Item;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.registries.ForgeRegistries;
import net.p3pp3rf1y.sophisticatedcore.Config;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class StackUpgradeConfig {
	private final ForgeConfigSpec.ConfigValue<List<String>> nonStackableItemsList;
	@Nullable
	private Set<Item> nonStackableItems = null;

	public StackUpgradeConfig(ForgeConfigSpec.Builder builder) {
		builder.comment("Stack Upgrade Settings").push("stackUpgrade");
		nonStackableItemsList = builder.comment("List of items that are not supposed to stack in storage even when stack upgrade is inserted. Item registry names are expected here.").define("nonStackableItems", new ArrayList<>());
		builder.pop();
	}

	public boolean canStackItem(Item item) {
		if (!Config.COMMON_SPEC.isLoaded()) {
			return true;
		}
		if (nonStackableItems == null) {
			nonStackableItems = new HashSet<>();
			nonStackableItemsList.get().forEach(name -> {
				ResourceLocation registryName = new ResourceLocation(name);
				if (ForgeRegistries.ITEMS.containsKey(registryName)) {
					nonStackableItems.add(ForgeRegistries.ITEMS.getValue(registryName));
				} else {
					SophisticatedCore.LOGGER.error("Item {} is set to not be affected by stack upgrade in config, but it does not exist in item registry", name);
				}
			});
		}
		return !nonStackableItems.contains(item);
	}

	public void clearNonStackableItems() {
		nonStackableItems = null;
	}
}
