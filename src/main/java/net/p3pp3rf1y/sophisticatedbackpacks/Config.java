package net.p3pp3rf1y.sophisticatedbackpacks;

import net.minecraftforge.common.ForgeConfigSpec;
import org.apache.commons.lang3.tuple.Pair;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
		Client(ForgeConfigSpec.Builder builder) {
			builder.comment("Client Settings").push("client");
			builder.pop();
		}
	}

	public static class Common {
		public final EnabledItems enabledItems;

		Common(ForgeConfigSpec.Builder builder) {
			builder.comment("Common Settings").push("common");

			enabledItems = new EnabledItems(builder);

			builder.pop();
		}

		public static class EnabledItems {
			private final ForgeConfigSpec.ConfigValue<List<String>> enabledItems;
			private final Map<String, Boolean> enabledMap = new HashMap<>();

			EnabledItems(ForgeConfigSpec.Builder builder) {
				enabledItems = builder.comment("Disable / enable any items here (disables their recipes)").define("enabledItems", new ArrayList<>());
			}

			public boolean isItemEnabled(String itemRegistryName) {
				if (enabledMap.isEmpty()) {
					loadEnabledMap();
				}
				if (!enabledMap.containsKey(itemRegistryName)) {
					enabledMap.put(itemRegistryName, true);
					addEnabledItemToConfig(itemRegistryName);
				}
				return enabledMap.get(itemRegistryName);
			}

			private void addEnabledItemToConfig(String itemRegistryName) {
				enabledItems.get().add(itemRegistryName + ":true");
				COMMON_SPEC.save();
			}

			private void loadEnabledMap() {
				for (String itemEnabled : enabledItems.get()) {
					String[] data = itemEnabled.split(":");
					if (data.length == 2) {
						enabledMap.put(data[0], Boolean.valueOf(data[1]));
					} else {
						SophisticatedBackpacks.LOGGER.error("Wrong data for enabledItems - expected name:true/false when {} was provided", itemEnabled);
					}
				}
			}
		}
	}
}
