package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.api.IUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.api.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.jukebox.JukeboxUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.SmeltingUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedbackpacks.upgrades.smelting.SmeltingUpgradeRenderer;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class UpgradeRenderRegistry {
	private UpgradeRenderRegistry() {}

	private static final Map<UpgradeRenderDataType<?>, IUpgradeRenderer<?>> UPGRADE_RENDERERS = new HashMap<>();

	private static <T extends IUpgradeRenderData> void registerUpgradeRenderer(UpgradeRenderDataType<T> upgradeRenderDataType, IUpgradeRenderer<T> upgradeRenderer) {
		UPGRADE_RENDERERS.put(upgradeRenderDataType, upgradeRenderer);
	}

	static {
		registerUpgradeRenderer(SmeltingUpgradeRenderData.TYPE, new SmeltingUpgradeRenderer());
		registerUpgradeRenderer(JukeboxUpgradeRenderData.TYPE, new JukeboxUpgradeRenderer());
	}

	public static <T extends IUpgradeRenderData> Optional<IUpgradeRenderer<T>> getUpgradeRenderer(UpgradeRenderDataType<T> upgradeRenderDataType) {
		//noinspection unchecked
		return Optional.ofNullable((IUpgradeRenderer<T>) UPGRADE_RENDERERS.get(upgradeRenderDataType));
	}
}
