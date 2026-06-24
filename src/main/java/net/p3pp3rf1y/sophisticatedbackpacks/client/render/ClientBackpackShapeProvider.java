package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.Minecraft;
import net.minecraft.world.level.block.Block;
import net.minecraft.world.level.block.state.BlockState;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackShapes;
import net.p3pp3rf1y.sophisticatedbackpacks.init.ModBlocks;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

public class ClientBackpackShapeProvider implements BackpackShapes.IShapeProvider {
	public static final ClientBackpackShapeProvider INSTANCE = new ClientBackpackShapeProvider();

	private volatile Map<BlockState, VoxelShape> shapeCache = Map.of();
	private final Set<Class<?>> warnedModelTypes = ConcurrentHashMap.newKeySet();
	private volatile boolean warnedRendererMissing = false;

	private ClientBackpackShapeProvider() {
	}

	@Override
	public VoxelShape getShape(BlockState state) {
		return shapeCache.getOrDefault(state, BackpackShapes.getDefaultShapeProvider().getShape(state));
	}

	public void rebuildShapes() {
		Minecraft minecraft = Minecraft.getInstance();
		try {
			minecraft.getModelManager().getBlockStateModelSet();
		} catch (NullPointerException | IllegalArgumentException ex) {
			if (!warnedRendererMissing) {
				warnedRendererMissing = true;
				SophisticatedBackpacks.LOGGER
						.warn("Backpack block models are not ready yet, using base model-derived shapes until client models are available");
			}
			shapeCache = Map.of();
			return;
		}
		warnedRendererMissing = false;

		Map<BlockState, VoxelShape> newShapes = new HashMap<>();
		List<Supplier<? extends Block>> backpackBlocks = List.of(ModBlocks.BACKPACK, ModBlocks.COPPER_BACKPACK, ModBlocks.IRON_BACKPACK,
				ModBlocks.GOLD_BACKPACK, ModBlocks.DIAMOND_BACKPACK, ModBlocks.NETHERITE_BACKPACK);
		for (Supplier<? extends Block> backpackBlockSupplier : backpackBlocks) {
			Block block = backpackBlockSupplier.get();
			for (BlockState state : block.getStateDefinition().getPossibleStates()) {
				newShapes.put(state, computeShapeFromLoadedModel(minecraft, state));
			}
		}

		shapeCache = Map.copyOf(newShapes);
	}

	private VoxelShape computeShapeFromLoadedModel(Minecraft minecraft, BlockState state) {
		Object model = minecraft.getModelManager().getBlockStateModelSet().get(state);
		return getDefaultShapeWithWarning(state, model);
	}

	private VoxelShape getDefaultShapeWithWarning(BlockState state, Object model) {
		Class<?> modelClass = model.getClass();
		if (warnedModelTypes.add(modelClass)) {
			SophisticatedBackpacks.LOGGER.warn("Backpack client model {} does not provide shape extraction, using base model-derived shape fallback",
					modelClass.getName());
		}
		VoxelShape shape = BackpackShapes.getDefaultShapeProvider().getShape(state);
		return shape.isEmpty() ? Shapes.block() : shape;
	}
}
