package net.p3pp3rf1y.sophisticatedcore.client.render;

import net.p3pp3rf1y.sophisticatedcore.api.IUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedcore.renderdata.IUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.renderdata.UpgradeRenderDataType;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.CookingUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.upgrades.cooking.CookingUpgradeRenderer;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeRenderData;
import net.p3pp3rf1y.sophisticatedcore.upgrades.jukebox.JukeboxUpgradeRenderer;

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
		registerUpgradeRenderer(CookingUpgradeRenderData.TYPE, new CookingUpgradeRenderer());
		registerUpgradeRenderer(JukeboxUpgradeRenderData.TYPE, new JukeboxUpgradeRenderer());
	}

	public static <T extends IUpgradeRenderData> Optional<IUpgradeRenderer<T>> getUpgradeRenderer(UpgradeRenderDataType<T> upgradeRenderDataType) {
		//noinspection unchecked
		return Optional.ofNullable((IUpgradeRenderer<T>) UPGRADE_RENDERERS.get(upgradeRenderDataType));
	}
}
