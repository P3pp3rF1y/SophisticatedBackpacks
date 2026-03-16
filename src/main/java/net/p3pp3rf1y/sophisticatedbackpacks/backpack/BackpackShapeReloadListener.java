package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.resources.Identifier;
import net.minecraft.server.packs.resources.ResourceManager;
import net.minecraft.server.packs.resources.SimplePreparableReloadListener;
import net.minecraft.util.profiling.ProfilerFiller;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;

public class BackpackShapeReloadListener extends SimplePreparableReloadListener<BackpackShapes.IShapeProvider> {
	public static final Identifier KEY = SophisticatedBackpacks.getIdentifier("backpack_shape_loader");
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
