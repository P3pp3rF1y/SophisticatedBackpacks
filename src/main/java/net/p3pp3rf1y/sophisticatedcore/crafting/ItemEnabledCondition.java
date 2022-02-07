package net.p3pp3rf1y.sophisticatedcore.crafting;

import com.google.gson.JsonObject;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.util.GsonHelper;
import net.minecraft.world.item.Item;
import net.minecraftforge.common.crafting.conditions.ICondition;
import net.minecraftforge.common.crafting.conditions.IConditionSerializer;
import net.p3pp3rf1y.sophisticatedcore.Config;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;

public class ItemEnabledCondition implements ICondition {
	private static final ResourceLocation NAME = SophisticatedCore.getRL("item_enabled");
	private final String itemRegistryName;

	public ItemEnabledCondition(Item item) {
		//noinspection ConstantConditions - only called after actually registered
		this(item.getRegistryName().getPath());
	}

	public ItemEnabledCondition(String itemRegistryName) {
		this.itemRegistryName = itemRegistryName;
	}

	@Override
	public ResourceLocation getID() {
		return NAME;
	}

	@Override
	public boolean test() {
		return Config.COMMON.enabledItems.isItemEnabled(itemRegistryName);
	}

	public static class Serializer implements IConditionSerializer<ItemEnabledCondition> {
		public static final Serializer INSTANCE = new Serializer();

		@Override
		public void write(JsonObject json, ItemEnabledCondition value) {
			json.addProperty("itemRegistryName", value.itemRegistryName);
		}

		@Override
		public ItemEnabledCondition read(JsonObject json) {
			return new ItemEnabledCondition(GsonHelper.getAsString(json, "itemRegistryName"));
		}

		@Override
		public ResourceLocation getID() {
			return NAME;
		}
	}
}
