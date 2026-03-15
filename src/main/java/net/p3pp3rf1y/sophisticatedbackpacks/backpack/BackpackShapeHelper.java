package net.p3pp3rf1y.sophisticatedbackpacks.backpack;

import net.minecraft.core.Direction;
import net.minecraft.world.phys.AABB;
import net.minecraft.world.phys.shapes.BooleanOp;
import net.minecraft.world.phys.shapes.Shapes;
import net.minecraft.world.phys.shapes.VoxelShape;

import java.util.function.Function;
import java.util.stream.Stream;

public final class BackpackShapeHelper {
	private BackpackShapeHelper() {
	}

	public enum Part {
		BASE,
		BATTERY,
		FRONT_POUCH,
		LEFT_POUCH,
		LEFT_TANK,
		RIGHT_POUCH,
		RIGHT_TANK
	}

	public record ShapeKey(Direction direction, boolean leftTank, boolean rightTank, boolean battery) {
		public static ShapeKey of(Direction direction, boolean leftTank, boolean rightTank, boolean battery) {
			return new ShapeKey(direction, leftTank, rightTank, battery);
		}
	}

	public static VoxelShape composeAndRotate(ShapeKey key, Function<Part, VoxelShape> partLookup) {
		VoxelShape composed = or(Stream.of(
				partLookup.apply(Part.BASE),
				partLookup.apply(key.leftTank ? Part.LEFT_TANK : Part.LEFT_POUCH),
				partLookup.apply(key.rightTank ? Part.RIGHT_TANK : Part.RIGHT_POUCH),
				partLookup.apply(key.battery ? Part.BATTERY : Part.FRONT_POUCH)
		));
		return rotateHorizontalFromNorth(composed, key.direction).optimize();
	}

	public static VoxelShape rotateHorizontalFromNorth(VoxelShape shape, Direction dir) {
		return switch (dir) {
			case NORTH -> shape;
			case EAST -> rotateShape(shape, (minX, minY, minZ, maxX, maxY, maxZ) -> box(1 - maxZ, minY, minX, 1 - minZ, maxY, maxX));
			case SOUTH -> rotateShape(shape, (minX, minY, minZ, maxX, maxY, maxZ) -> box(1 - maxX, minY, 1 - maxZ, 1 - minX, maxY, 1 - minZ));
			case WEST -> rotateShape(shape, (minX, minY, minZ, maxX, maxY, maxZ) -> box(minZ, minY, 1 - maxX, maxZ, maxY, 1 - minX));
			default -> shape;
		};
	}

	public static VoxelShape or(Stream<VoxelShape> shapes) {
		return shapes.reduce((v1, v2) -> Shapes.joinUnoptimized(v1, v2, BooleanOp.OR))
				.orElse(Shapes.empty());
	}

	private static VoxelShape rotateShape(VoxelShape shape, DoubleLineFunction rotate) {
		VoxelShape ret = Shapes.empty();
		for (AABB aabb : shape.toAabbs()) {
			ret = Shapes.joinUnoptimized(ret, rotate.apply(aabb.minX, aabb.minY, aabb.minZ, aabb.maxX, aabb.maxY, aabb.maxZ), BooleanOp.OR);
		}
		return ret;
	}

	private static VoxelShape box(double minX, double minY, double minZ, double maxX, double maxY, double maxZ) {
		return Shapes.box(Math.min(minX, maxX), Math.min(minY, maxY), Math.min(minZ, maxZ), Math.max(minX, maxX), Math.max(minY, maxY), Math.max(minZ, maxZ));
	}

	private interface DoubleLineFunction {
		VoxelShape apply(double minX, double minY, double minZ, double maxX, double maxY, double maxZ);
	}
}
