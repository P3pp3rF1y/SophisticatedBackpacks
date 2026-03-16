package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.server.packs.resources.SimplePreparableReloadListener;
import net.minecraft.util.profiling.ProfilerFiller;

public class BackpackShapeReloadListener extends SimplePreparableReloadListener<BackpackShapes.IShapeProvider> {
	public static final BackpackShapeReloadListener INSTANCE = new BackpackShapeReloadListener();

	private BackpackShapeReloadListener() {
	}

	@Override
	protected BackpackShapes.IShapeProvider prepare(ResourceManager resourceManager, ProfilerFiller profiler) {
		return BackpackShapes.createDefaultShapeProvider(resourceManager);
	}

	@Override
	protected void apply(BackpackShapes.IShapeProvider provider, ResourceManager resourceManager, ProfilerFiller profiler) {
		BackpackShapes.applyDefaultShapeProvider(provider);
	}
}
