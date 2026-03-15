package net.p3pp3rf1y.sophisticatedbackpacks.client.render;

import net.minecraft.client.Minecraft;
import net.minecraft.client.resources.model.BakedModel;
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

import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.BATTERY;
import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.FACING;
import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.LEFT_TANK;
import static net.p3pp3rf1y.sophisticatedbackpacks.backpack.BackpackBlock.RIGHT_TANK;

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
		if (minecraft.getBlockRenderer() == null) {
			if (!warnedRendererMissing) {
				warnedRendererMissing = true;
				SophisticatedBackpacks.LOGGER.warn("Backpack block renderer is not ready yet, using base model-derived shapes until client models are available");
			}
			shapeCache = Map.of();
			return;
		}
		warnedRendererMissing = false;

		Map<BlockState, VoxelShape> newShapes = new HashMap<>();
		List<Supplier<? extends Block>> backpackBlocks = List.of(
				ModBlocks.BACKPACK,
				ModBlocks.COPPER_BACKPACK,
				ModBlocks.IRON_BACKPACK,
				ModBlocks.GOLD_BACKPACK,
				ModBlocks.DIAMOND_BACKPACK,
				ModBlocks.NETHERITE_BACKPACK
		);
		for (Supplier<? extends Block> backpackBlockSupplier : backpackBlocks) {
			Block block = backpackBlockSupplier.get();
			for (BlockState state : block.getStateDefinition().getPossibleStates()) {
				newShapes.put(state, computeShapeFromLoadedModel(minecraft, state));
			}
		}

		shapeCache = Map.copyOf(newShapes);
	}

	private VoxelShape computeShapeFromLoadedModel(Minecraft minecraft, BlockState state) {
		BakedModel model = minecraft.getBlockRenderer().getBlockModel(state);
		if (model instanceof BackpackDynamicModel.BackpackBakedModel backpackModel) {
			return backpackModel.getShape(state.getValue(FACING), state.getValue(LEFT_TANK), state.getValue(RIGHT_TANK), state.getValue(BATTERY))
					.orElseGet(() -> getDefaultShapeWithWarning(state, model));
		}
		return getDefaultShapeWithWarning(state, model);
	}

	private VoxelShape getDefaultShapeWithWarning(BlockState state, BakedModel model) {
		Class<?> modelClass = model.getClass();
		if (warnedModelTypes.add(modelClass)) {
			SophisticatedBackpacks.LOGGER.warn("Backpack client model {} does not provide shape extraction, using base model-derived shape fallback", modelClass.getName());
		}
		VoxelShape shape = BackpackShapes.getDefaultShapeProvider().getShape(state);
		return shape.isEmpty() ? Shapes.block() : shape;
	}
}
