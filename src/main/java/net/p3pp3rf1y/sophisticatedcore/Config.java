package net.p3pp3rf1y.sophisticatedcore;

import net.minecraft.world.item.Item;
import net.minecraftforge.common.ForgeConfigSpec;
import net.minecraftforge.fml.event.config.ModConfigEvent;
import net.p3pp3rf1y.sophisticatedcore.client.gui.SortButtonsPosition;
import net.p3pp3rf1y.sophisticatedcore.util.RegistryHelper;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Config {

	private Config() {}

	public static final Client CLIENT;
	public static final ForgeConfigSpec CLIENT_SPEC;
	public static final Common COMMON;
	public static final ForgeConfigSpec COMMON_SPEC;

	static {
		final Pair<Client, ForgeConfigSpec> clientSpec = new ForgeConfigSpec.Builder().configure(Client::new);
		CLIENT_SPEC = clientSpec.getRight();
		CLIENT = clientSpec.getLeft();

		final Pair<Common, ForgeConfigSpec> commonSpec = new ForgeConfigSpec.Builder().configure(Common::new);
		COMMON_SPEC = commonSpec.getRight();
		COMMON = commonSpec.getLeft();
	}

	public static class Client {
		public final ForgeConfigSpec.EnumValue<SortButtonsPosition> sortButtonsPosition;
		public final ForgeConfigSpec.BooleanValue playButtonSound;

		Client(ForgeConfigSpec.Builder builder) {
			builder.comment("Client Settings").push("client");
			sortButtonsPosition = builder.comment("Positions where sort buttons can display to help with conflicts with controls from other mods").defineEnum("sortButtonsPosition", SortButtonsPosition.TITLE_LINE_RIGHT);
			playButtonSound = builder.comment("Whether click sound should play when custom buttons are clicked in gui").define("playButtonSound", true);
			builder.pop();
		}
	}

	public static class Common {
		public final EnabledItems enabledItems;

		@SuppressWarnings("unused") //need the Event parameter for forge reflection to understand what event this listens to
		public void onConfigReload(ModConfigEvent.Reloading event) {
			enabledItems.enabledMap.clear();
		}

		Common(ForgeConfigSpec.Builder builder) {
			builder.comment("Common Settings").push("common");

			enabledItems = new EnabledItems(builder);
		}

		public static class EnabledItems {
			private final ForgeConfigSpec.ConfigValue<List<String>> itemsEnableList;
			private final Map<String, Boolean> enabledMap = new ConcurrentHashMap<>();

			EnabledItems(ForgeConfigSpec.Builder builder) {
				itemsEnableList = builder.comment("Disable / enable any items here (disables their recipes)").define("enabledItems", new ArrayList<>());
			}

			public boolean isItemEnabled(Item item) {
				return RegistryHelper.getRegistryName(item).map(rn -> isItemEnabled(rn.getPath())).orElse(false);
			}

			public boolean isItemEnabled(String itemRegistryName) {
				if (!COMMON_SPEC.isLoaded()) {
					return true;
				}
				if (enabledMap.isEmpty()) {
					loadEnabledMap();
				}
				return enabledMap.computeIfAbsent(itemRegistryName, irn -> {
					addEnabledItemToConfig(itemRegistryName);
					return true;
				});
			}

			private void addEnabledItemToConfig(String itemRegistryName) {
				itemsEnableList.get().add(itemRegistryName + ":true");
				COMMON_SPEC.save();
			}

			private void loadEnabledMap() {
				for (String itemEnabled : itemsEnableList.get()) {
					String[] data = itemEnabled.split(":");
					if (data.length == 2) {
						enabledMap.put(data[0], Boolean.valueOf(data[1]));
					} else {
						SophisticatedCore.LOGGER.error("Wrong data for enabledItems - expected name:true/false when {} was provided", itemEnabled);
					}
				}
			}
		}

	}
}
